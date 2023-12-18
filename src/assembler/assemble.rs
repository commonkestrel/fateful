use std::ops::Deref;
use std::sync::Arc;

use crate::{
    assembler::{
        eval,
        lex::{Ident, Span, Token, TokenInner},
        parse::DSeg,
        token::Immediate,
    },
    diagnostic::Reference,
    spanned_error, Token,
};

use super::{
    lex::Register,
    lex::TokenStream,
    parse::{
        Argument, Bracketed, CSeg, Inst, Label, Macro, Parenthesized, ParseStream, ParseTok, Types,
    },
    Diagnostic, Errors,
};

use std::collections::HashMap;

use phf::{phf_map, Map};

const IMMEDIATE_MASK: u8 = 0b0000_1000;
const ADD: u8 = 0x00;
const SUB: u8 = 0x10;
const ADC: u8 = 0x20;
const SBC: u8 = 0x30;
const NAND: u8 = 0x40;
const OR: u8 = 0x50;
const CMP: u8 = 0x60;
const MV: u8 = 0x70;
const LD: u8 = 0x80;
const ST: u8 = 0x90;
const LDA: u8 = 0xA0;
const LPM: u8 = 0xB0;
const PUSH: u8 = 0xC0;
const POP: u8 = 0xD0;
const JNZ: u8 = 0xE0;
const HALT: u8 = 0xF0;

static INSTRUCTIONS: Instructions = Instructions::new(phf_map! {
    "add" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "sub" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "adc" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "sbc" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "nand" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "or" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "cmp" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "mv" => &[&[Types::REG, Types::REG.union(Types::IMM)]],
    "ld" => &[&[Types::REG], &[Types::REG, Types::ADDR]],
    "st" => &[&[Types::REG], &[Types::ADDR, Types::REG]],
    "lda" => &[&[Types::ADDR.union(Types::LABEL)]],
    "lpm" => &[&[Types::REG], &[Types::REG, Types::LABEL]],
    "push" => &[&[Types::REG.union(Types::IMM)]],
    "pop" => &[&[Types::REG]],
    "jnz" => &[&[Types::REG.union(Types::IMM)]],
    "halt" => &[&[]],
});

#[repr(transparent)]
struct Instructions {
    matches: Map<&'static str, &'static [&'static [Types]]>,
}

impl Instructions {
    const fn new(matches: Map<&'static str, &'static [&'static [Types]]>) -> Self {
        Instructions { matches }
    }

    fn matches(&self, inst: &Inst) -> bool {
        if let Some(builtin) = INSTRUCTIONS.matches.get(&inst.name.value) {
            builtin.iter().any(|matches| {
                if matches.len() != inst.args.len() {
                    return false;
                }

                inst.args
                    .values()
                    .zip(matches.iter())
                    .all(|(argument, types)| match argument {
                        Argument::Addr(_) => types.intersects(Types::LABEL | Types::ADDR),
                        Argument::Expr(_) | Argument::Immediate(_) => types.contains(Types::IMM),
                        Argument::Reg(_) => types.contains(Types::REG),
                        Argument::Ident(_) => types.contains(Types::IDENT),
                        Argument::Str(_) => types.contains(Types::STR),
                    })
            })
        } else {
            false
        }
    }
}

enum Instruction {
    Add(Register, RegImm),
    Sub(Register, RegImm),
    Adc(Register, RegImm),
    Sbc(Register, RegImm),
    Nand(Register, RegImm),
    Or(Register, RegImm),
    Cmp(Register, RegImm),
    Mv(Register, RegImm),
    LdHl(Register),
    LdAddr(Register, Bracketed<TokenStream>),
    StHl(Register),
    StAddr(Bracketed<TokenStream>, Register),
    Lda(Bracketed<TokenStream>),
    LpmHl(Register),
    LpmAddr(Register, Bracketed<TokenStream>),
    Push(RegImm),
    Pop(Register),
    Jnz(RegImm),
    Halt,
}

impl Instruction {
    fn size(&self) -> u16 {
        match self {
            Instruction::Add(_, _) => 2,
            Instruction::Sub(_, _) => 2,
            Instruction::Adc(_, _) => 2,
            Instruction::Sbc(_, _) => 2,
            Instruction::Nand(_, _) => 2,
            Instruction::Or(_, _) => 2,
            Instruction::Cmp(_, _) => 2,
            Instruction::Mv(_, _) => 2,
            Instruction::LdHl(_) => 1,
            Instruction::LdAddr(_, _) => 3,
            Instruction::StHl(_) => 1,
            Instruction::StAddr(_, _) => 3,
            Instruction::Lda(_) => 3,
            Instruction::LpmHl(_) => 1,
            Instruction::LpmAddr(_, _) => 3,
            Instruction::Push(regimm) => match regimm {
                RegImm::Immediate(_) | RegImm::Expr(_) => 2,
                RegImm::Register(_) => 1,
            },
            Instruction::Pop(_) => 1,
            Instruction::Jnz(regimm) => match regimm {
                RegImm::Immediate(_) | RegImm::Expr(_) => 2,
                RegImm::Register(_) => 1,
            },
            Instruction::Halt => 1,
        }
    }

    fn compile(
        self,
        pc: u16,
        data: &HashMap<String, (u16, Arc<Span>)>,
        labels: &HashMap<String, (u16, Arc<Span>)>,
    ) -> Result<Bytes, Diagnostic> {
        match self {
            Instruction::Add(reg, regimm) => {
                Instruction::compile_double(ADD, reg, regimm, labels, data)
            }
            Instruction::Sub(reg, regimm) => {
                Instruction::compile_double(SUB, reg, regimm, labels, data)
            }
            Instruction::Adc(reg, regimm) => {
                Instruction::compile_double(ADC, reg, regimm, labels, data)
            }
            Instruction::Sbc(reg, regimm) => {
                Instruction::compile_double(SBC, reg, regimm, labels, data)
            }
            Instruction::Nand(reg, regimm) => {
                Instruction::compile_double(NAND, reg, regimm, labels, data)
            }
            Instruction::Or(reg, regimm) => {
                Instruction::compile_double(OR, reg, regimm, labels, data)
            }
            Instruction::Cmp(reg, regimm) => {
                Instruction::compile_double(CMP, reg, regimm, labels, data)
            }
            Instruction::Mv(reg, regimm) => {
                Instruction::compile_double(MV, reg, regimm, labels, data)
            }
            Instruction::Halt => Ok(Bytes::Single([HALT])),
            _ => todo!(),
        }
    }

    fn compile_double(
        instruction: u8,
        reg: Register,
        regimm: RegImm,
        labels: &HashMap<String, (u16, Arc<Span>)>,
        data: &HashMap<String, (u16, Arc<Span>)>,
    ) -> Result<Bytes, Diagnostic> {
        match regimm {
            RegImm::Register(second) => Ok(Bytes::Double([ADD | reg as u8, second as u8])),
            RegImm::Immediate(imm) => Ok(Bytes::Double([ADD | IMMEDIATE_MASK | reg as u8, imm])),
            RegImm::Expr(expr) => {
                let expr_span = Span::same_line(&expr.open.span, &expr.close.span);
                Ok(Bytes::Double([
                    instruction | IMMEDIATE_MASK | reg as u8,
                    eval::eval_expr(expr, labels, data)?
                        .value
                        .try_into()
                        .map_err(|_| spanned_error!(expr_span, "immediate out of range"))?,
                ]))
            }
        }
    }
}

impl TryFrom<Inst> for Instruction {
    type Error = Diagnostic;

    fn try_from(mut value: Inst) -> Result<Self, Self::Error> {
        let mut args = value.args.into_values();
        match value.name.value.as_str() {
            "add" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Add(reg, regimm))
            }
            "sub" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Sub(reg, regimm))
            }
            "adc" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Adc(reg, regimm))
            }
            "sbc" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Sbc(reg, regimm))
            }
            "nand" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Nand(reg, regimm))
            }
            "or" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Or(reg, regimm))
            }
            "cmp" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Cmp(reg, regimm))
            }
            "mv" => {
                if args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 2 arguments, found {}",
                        args.len()
                    ));
                }
                let (reg, regimm) = pull_double(args)?;
                Ok(Instruction::Mv(reg, regimm))
            }
            "ld" => {
                if args.len() != 1 && args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 1 or 2 arguments, found {}",
                        args.len()
                    ));
                }

                let reg = match args.swap_remove(0) {
                    Argument::Reg(reg) => reg.inner,
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected register, found {}",
                            arg.description()
                        ))
                    }
                };

                if args.is_empty() {
                    Ok(Instruction::LdHl(reg))
                } else {
                    let addr = match args.swap_remove(0) {
                        Argument::Addr(addr) => addr,
                        arg => {
                            return Err(spanned_error!(
                                arg.span(),
                                "expected address, found {}",
                                arg.description()
                            ))
                        }
                    };

                    Ok(Instruction::LdAddr(reg, addr))
                }
            }
            "st" => match args.len() {
                1 => {
                    let reg = match args.swap_remove(0) {
                        Argument::Reg(reg) => reg.inner,
                        arg => {
                            return Err(spanned_error!(
                                arg.span(),
                                "expected register, found {}",
                                arg.description(),
                            ))
                        }
                    };

                    Ok(Instruction::StHl(reg))
                }
                2 => {
                    let addr = match args.swap_remove(0) {
                        Argument::Addr(addr) => addr,
                        arg => {
                            return Err(spanned_error!(
                                arg.span(),
                                "expected address, found {}",
                                arg.description(),
                            ))
                        }
                    };

                    let reg = match args.swap_remove(0) {
                        Argument::Reg(reg) => reg.inner,
                        arg => {
                            return Err(spanned_error!(
                                arg.span(),
                                "expected register, found {}",
                                arg.description(),
                            ))
                        }
                    };

                    Ok(Instruction::StAddr(addr, reg))
                }
                len => Err(spanned_error!(
                    value.name.span,
                    "expected 1 or 2 arguments, found {len}"
                )),
            },
            "lda" => {
                if args.len() != 1 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 1 argument, found {}",
                        args.len()
                    ));
                }

                let addr = match args.swap_remove(0) {
                    Argument::Addr(addr) => addr,
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected address, found {}",
                            arg.description(),
                        ))
                    }
                };

                Ok(Instruction::Lda(addr))
            }
            "lpm" => {
                if args.len() != 1 && args.len() != 2 {
                    return Err(spanned_error!(
                        value.name.span,
                        "expected 1 or 2 arguments, found {}",
                        args.len()
                    ));
                }

                let reg = match args.swap_remove(0) {
                    Argument::Reg(reg) => reg.inner,
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected register, found {}",
                            arg.description()
                        ))
                    }
                };

                if args.is_empty() {
                    Ok(Instruction::LpmHl(reg))
                } else {
                    let addr = match args.swap_remove(0) {
                        Argument::Addr(addr) => addr,
                        arg => {
                            return Err(spanned_error!(
                                arg.span(),
                                "expected address, found {}",
                                arg.description()
                            ))
                        }
                    };

                    Ok(Instruction::LpmAddr(reg, addr))
                }
            }
            "push" => {
                let regimm = match args.swap_remove(0) {
                    Argument::Reg(reg) => RegImm::Register(reg.inner),
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected register or immediate, found {}",
                            arg.description()
                        ))
                    }
                };

                Ok(Instruction::Push(regimm))
            }
            "pop" => {
                let reg = match args.swap_remove(0) {
                    Argument::Reg(reg) => reg.inner,
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected register, found {}",
                            arg.description()
                        ))
                    }
                };

                Ok(Instruction::Pop(reg))
            }
            "jnz" => {
                let regimm = match args.swap_remove(0) {
                    Argument::Reg(reg) => RegImm::Register(reg.inner),
                    arg => {
                        return Err(spanned_error!(
                            arg.span(),
                            "expected register or immediate, found {}",
                            arg.description()
                        ))
                    }
                };

                Ok(Instruction::Jnz(regimm))
            }
            "halt" => match args.len() {
                0 => Ok(Instruction::Halt),
                len => Err(spanned_error!(
                    value.name.span,
                    "expected 0 arguments, found {len}"
                )),
            },
            _ => Err(spanned_error!(value.name.span, "unknown instruction")),
        }
    }
}

/// # Panics
///
/// Panics if the arguments are not an immediate followed by a register or immediate.
/// Should be guarded by the calls to [`Instructions::matches`] in [`expand_macros`]
fn pull_double(mut args: Vec<Argument>) -> Result<(Register, RegImm), Diagnostic> {
    let reg = match args.swap_remove(0) {
        Argument::Reg(reg) => reg.inner,
        arg => {
            return Err(spanned_error!(
                arg.span(),
                "expected register, found {}",
                arg.description()
            ))
        }
    };

    let regimm = match args.swap_remove(0) {
        Argument::Reg(reg) => RegImm::Register(reg.inner),
        arg => {
            return Err(spanned_error!(
                arg.span(),
                "expected register or immediate, found {}",
                arg.description()
            ))
        }
    };

    Ok((reg, regimm))
}

/// # Panics
///
/// Panics if the provided argument is not of variant [`Argument::Addr`].
/// Should be guarded by the calls to [`Instructions::matches`] in [`expand_macros`]
fn pull_address(
    arg: Argument,
    data: &HashMap<String, (u16, Arc<Span>)>,
) -> Result<u16, Diagnostic> {
    match arg {
        Argument::Addr(addr) => {
            for tok in addr.inner.iter() {
                match tok.inner {
                    TokenInner::Ident(Ident::Ident(_)) | TokenInner::Location => {
                        return Err(spanned_error!(
                            tok.span.clone(),
                            "unexpected program reference in memory address"
                        ));
                    }
                    _ => {}
                }
            }

            let evaluated = eval::eval_bracketed(addr, data)?;
            evaluated
                .value
                .try_into()
                .map_err(|_| spanned_error!(evaluated.span, "memory address out of range"))
        }
        _ => unreachable!(),
    }
}

fn pull_either(
    arg: Argument,
    data: &HashMap<String, (u16, Arc<Span>)>,
    labels: &HashMap<String, (u16, Arc<Span>)>,
    pc: u16,
) -> Result<u16, Diagnostic> {
    match arg {
        Argument::Addr(mut addr) => {
            let mut mem = None;
            let mut prog = None;

            for tok in addr.inner.iter_mut() {
                if let Token {
                    span,
                    inner: TokenInner::Ident(Ident::Ident(val)),
                } = tok
                {
                    if let Some(prev) = mem {
                        return Err(Diagnostic::referencing_error(
                            span.clone(),
                            "unexpected label in memory address",
                            Reference::new(
                                prev,
                                "interpreted as memory address due to this reference",
                            ),
                        ));
                    }
                    prog.get_or_insert(span.clone());

                    let label = labels.get(val).ok_or_else(|| {
                        spanned_error!(span.clone(), "unrecognized identifier in expression")
                    })?;
                    tok.inner = TokenInner::Immediate(label.0 as i128);
                } else if let Token {
                    span,
                    inner: TokenInner::Ident(Ident::Variable(var)),
                } = tok
                {
                    if let Some(prev) = prog {
                        return Err(Diagnostic::referencing_error(
                            span.clone(),
                            "unexpected variable in program address",
                            Reference::new(
                                prev,
                                "interpreted as program address due to this reference",
                            ),
                        ));
                    }
                    mem.get_or_insert(span.clone());

                    let variable = data.get(var).ok_or_else(|| {
                        spanned_error!(span.clone(), "variable not found in scope")
                    })?;
                    tok.inner = TokenInner::Immediate(variable.0 as i128);
                } else if let Token {
                    span,
                    inner: TokenInner::Location,
                } = tok
                {
                    if let Some(prev) = mem {
                        return Err(Diagnostic::referencing_error(
                            span.clone(),
                            "unexpected program location in memory address",
                            Reference::new(
                                prev,
                                "interpreted as memory address due to this reference",
                            ),
                        ));
                    }
                    prog.get_or_insert(span.clone());

                    tok.inner = TokenInner::Immediate(pc as i128);
                }
            }

            let evaled = eval::eval_bracketed(addr, &HashMap::new())?;
            evaled
                .value
                .try_into()
                .map_err(|_| spanned_error!(evaled.span, "address not in range"))
        }
        _ => unreachable!(),
    }
}

/// # Panics
///
/// Panics if the provided argument is not of variant [`Argument::Addr`].
/// Should be guarded by the calls to [`Instructions::matches`] in [`expand_macros`]
fn pull_reference(
    arg: Argument,
    data: &HashMap<String, (u16, Arc<Span>)>,
) -> Result<u16, Diagnostic> {
    match arg {
        Argument::Addr(addr) => {
            for tok in addr.inner.iter() {
                match tok.inner {
                    TokenInner::Ident(Ident::Variable(_)) => {
                        return Err(spanned_error!(
                            tok.span.clone(),
                            "unexpected variable in program address"
                        ));
                    }
                    _ => {}
                }
            }

            let evaluated = eval::eval_bracketed(addr, data)?;
            evaluated
                .value
                .try_into()
                .map_err(|_| spanned_error!(evaluated.span, "program address out of range"))
        }
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy)]
enum Bytes {
    Single([u8; 1]),
    Double([u8; 2]),
    Triple([u8; 3]),
}

impl Deref for Bytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Bytes::Single(byte) => byte.as_slice(),
            Bytes::Double(bytes) => bytes.as_slice(),
            Bytes::Triple(bytes) => bytes.as_slice(),
        }
    }
}

fn compile(
    stream: Vec<ExpSeg>,
    data: HashMap<String, (u16, Arc<Span>)>,
) -> Result<[u8; 1 << 16], Errors> {
    let mut program = [0; 1 << 16];
    let mut errors = Errors::new();
    let mut labels = HashMap::new();
    let mut pc: u16 = 0;

    for segment in stream {
        pc = match segment.origin(pc) {
            Ok(origin) => origin,
            Err(err) => {
                errors.push(err);
                pc
            }
        };
        let start = pc;

        for expr in segment.instructions {
            match expr {
                ExpTok::Instruction(inst) => {
                    let inst = match inst.compile(pc, &data, &labels) {
                        Ok(inst) => inst,
                        Err(err) => {
                            errors.push(err);
                            continue;
                        }
                    };

                    for byte in inst.into_iter() {
                        program[pc as usize] = *byte;
                        pc += 1;
                    }
                }
                ExpTok::Label(label) => {
                    if !label.name.value.starts_with('.') {
                        labels.retain(|key, _| !key.starts_with('.'))
                    }

                    let span = label.name.span.clone();
                    if let Some((_, prev_span)) =
                        labels.insert(label.name.value, (pc, label.name.span))
                    {
                        errors.push(Diagnostic::referencing_error(
                            span,
                            "duplicate label definitions",
                            Reference::new(prev_span, "previous definition found here"),
                        ))
                    }
                }
                ExpTok::Bytes(bytes) => {
                    for byte in bytes {
                        program[pc as usize] = byte;
                        pc += 1;
                    }
                }
            }
        }

        let range = start..pc;
    }

    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}

enum RegImm {
    Immediate(u8),
    Expr(Parenthesized<TokenStream>),
    Register(Register),
}

pub fn assemble_data(stream: Vec<DSeg>) -> Result<HashMap<String, (u16, Arc<Span>)>, Errors> {
    let mut variables = HashMap::new();
    let mut errors = Errors::new();
    let mut ranges: Vec<std::ops::Range<u16>> = Vec::new();
    let mut ptr = 0x0000;

    for segment in stream.iter() {
        let size = match segment.size() {
            Ok(s) => s,
            Err(err) => {
                errors.push(err);
                continue;
            }
        };
        let origin = match segment.org {
            Some(ref imm) => match imm
                .value
                .try_into()
                .map_err(|_| spanned_error!(imm.span.clone(), "segment origin out of range"))
            {
                Ok(org) => org,
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            },
            None => ptr,
        };

        let segment_range = origin..(origin + size);

        for (i, range) in ranges.iter().enumerate() {
            if segment_range.start.max(range.start) <= segment_range.end.min(range.end) {
                errors.push(
                    Diagnostic::referencing_error(
                        segment.dseg.span.clone(),
                        "data segment collision",
                        Reference::new(
                            stream[i].dseg.span.clone(),
                            "overlaps with this segment")
                    )
                    .with_help("segments of the same type cannot overlap; try adjusting the origin or variable sizes")
                )
            }
        }

        ranges.push(segment_range);

        for (name, (variable, span)) in segment.variables.iter() {
            let span = span.clone();
            if let Some((_, prev)) = variables.insert(name.to_owned(), (ptr, span.clone())) {
                errors.push(Diagnostic::referencing_error(
                    span,
                    "duplicate variable definition",
                    Reference::new(prev, "variable previously defined here"),
                ))
            }
            ptr += variable;
        }
    }

    if errors.is_empty() {
        Ok(variables)
    } else {
        Err(errors)
    }
}

fn expand_macro(inst: Inst, def: &Macro) -> Result<Vec<ParseTok>, Diagnostic> {
    let span = inst
        .args
        .fl()
        .map(|(first, last)| {
            let first_span = first.span();
            Arc::new(Span {
                line: first_span.line,
                source: first_span.source.clone(),
                range: first_span.start()..last.span().end(),
            })
        })
        .unwrap_or(inst.name.span.clone());

    def.expand(span, &inst.args.into_values())
}

fn expand_macros(code: Vec<CSeg>, macros: HashMap<String, Macro>) -> Result<Vec<ExpSeg>, Errors> {
    let mut errors = Errors::new();
    let mut segments = Vec::new();

    for mut segment in code {
        let mut position = 0;
        let mut exp = ExpSeg {
            cseg: segment.cseg,
            org: segment.org,
            instructions: Vec::new(),
        };

        while let Some(expr) = segment.tokens.get(position) {
            if let ParseTok::Instruction(inst) = expr {
                match Instruction::try_from(inst.clone()) {
                    Ok(instruction) => exp.instructions.push(ExpTok::Instruction(instruction)),
                    Err(err) => match macros.get(&inst.name.value) {
                        Some(def) => match expand_macro(inst.clone(), def) {
                            Ok(expanded) => {
                                segment.tokens.splice(position..=position, expanded);
                            }
                            Err(_) => errors.push(err),
                        },
                        None => errors.push(err),
                    },
                }
            }
            position += 1;
        }
    }

    if errors.is_empty() {
        Ok(segments)
    } else {
        Err(errors)
    }
}

enum ExpTok {
    Instruction(Instruction),
    Label(Label),
    Bytes(Vec<u8>),
}

struct ExpSeg {
    cseg: Option<Token![@cseg]>,
    org: Option<Immediate>,
    instructions: Vec<ExpTok>,
}

impl ExpSeg {
    pub fn origin(&self, default: u16) -> Result<u16, Diagnostic> {
        self.org
            .as_ref()
            .map(|org| {
                org.value
                    .try_into()
                    .map_err(|_| spanned_error!(org.span.clone(), "segment origin out of range"))
            })
            .unwrap_or(Ok(default))
    }
}

pub fn assemble(mut ctx: ParseStream) -> Result<[u8; 1 << 16], Errors> {
    let data = assemble_data(ctx.data)?;
    let expanded = expand_macros(ctx.code, ctx.macros)?;
    compile(expanded, data)
}
