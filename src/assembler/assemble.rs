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
    note, spanned_note, spanned_error, Token,
};

use super::{
    parse::{Address, Argument, CSeg, ExpTok, Inst, Macro, ParseStream, Punctuated, Types},
    token::Register,
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

fn compile_instruction(
    inst: Inst,
    pc: u16,
    data: &HashMap<String, (u16, Arc<Span>)>,
    labels: &HashMap<String, (u16, Arc<Span>)>,
) -> Result<Instruction, Diagnostic> {    
    let mut arguments = inst.args.into_values();
    for arg in arguments.iter_mut() {
        match arg {
            Argument::Addr(addr) => {
                for tok in addr.inner.iter_mut() {
                    if let TokenInner::Location= tok.inner {
                        tok.inner = TokenInner::Immediate(pc as i128);
                    }
                }
            }
            Argument::Expr(expr) => {
                for tok in expr.inner.iter_mut() {
                    if let TokenInner::Location = tok.inner {
                        tok.inner = TokenInner::Immediate(pc as i128);
                    }
                }
            }
            _ => {}
        }
    }

    match inst.name.value.as_str() {
        "add" => pull_double(arguments, ADD),
        "sub" => pull_double(arguments, SUB),
        "adc" => pull_double(arguments, ADC),
        "sbc" => pull_double(arguments, SBC),
        "nand" => pull_double(arguments, NAND),
        "or" => pull_double(arguments, OR),
        "cmp" => pull_double(arguments, CMP),
        "mv" => pull_double(arguments, MV),
        "ld" => {
            let first: u8 = match arguments.swap_remove(0) {
                Argument::Reg(reg) => reg.inner as u8,
                _ => unreachable!(),
            };

            if arguments.is_empty() {
                Ok(Instruction::Single([LD | IMMEDIATE_MASK | first]))
            } else {
                let second = pull_address(arguments.swap_remove(0), data)?;
                Ok(Instruction::Triple([
                    LD | IMMEDIATE_MASK | first,
                    (second >> 8) as u8,
                    (second & 0xFF) as u8,
                ]))
            }
        }
        "st" => match arguments.swap_remove(0) {
            Argument::Reg(reg) => Ok(Instruction::Single([ST | reg.inner as u8])),
            addr => {
                let address = pull_address(addr, data)?;
                let reg = match arguments.swap_remove(0) {
                    Argument::Reg(reg) => reg.inner,
                    _ => unreachable!(),
                };

                Ok(Instruction::Triple([
                    ST | IMMEDIATE_MASK | reg as u8,
                    (address >> 8) as u8,
                    (address & 0xFF) as u8,
                ]))
            }
        },
        "lda" => {
            let addr = pull_either(arguments.swap_remove(0), data, labels, pc)?;
            
            Ok(Instruction::Triple([
                LDA | IMMEDIATE_MASK,
                (addr >> 8) as u8,
                (addr & 0xFF) as u8,
            ]))
        }
        "lpm" => {
            let first: u8 = match arguments.swap_remove(0) {
                Argument::Reg(reg) => reg.inner as u8,
                _ => unreachable!(),
            };

            if arguments.is_empty() {
                Ok(Instruction::Single([LD | IMMEDIATE_MASK | first]))
            } else {
                let second = pull_reference(arguments.swap_remove(0), labels)?;
                Ok(Instruction::Triple([
                    LD | IMMEDIATE_MASK | first,
                    (second >> 8) as u8,
                    (second & 0xFF) as u8,
                ]))
            }
        }
        "push" => match arguments.swap_remove(0) {
            Argument::Immediate(imm) => Ok(Instruction::Double([
                PUSH | IMMEDIATE_MASK,
                imm.value
                    .try_into()
                    .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
            ])),
            Argument::Expr(expr) => {
                let imm = eval::eval_expr(expr)?;
                Ok(Instruction::Double([
                    PUSH | IMMEDIATE_MASK,
                    imm.value
                        .try_into()
                        .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
                ]))
            }
            Argument::Reg(reg) => Ok(Instruction::Single([PUSH | reg.inner as u8])),
            _ => unreachable!(),
        },
        "pop" => match arguments.swap_remove(0) {
            Argument::Reg(reg) => Ok(Instruction::Single([POP | reg.inner as u8])),
            _ => unreachable!(),
        },
        "jnz" => match arguments.swap_remove(0) {
            Argument::Immediate(imm) => Ok(Instruction::Double([
                JNZ | IMMEDIATE_MASK,
                imm.value
                    .try_into()
                    .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
            ])),
            Argument::Expr(expr) => {
                let imm = eval::eval_expr(expr)?;
                Ok(Instruction::Double([
                    JNZ | IMMEDIATE_MASK,
                    imm.value
                        .try_into()
                        .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
                ]))
            }
            Argument::Reg(reg) => Ok(Instruction::Single([JNZ | reg.inner as u8])),
            _ => unreachable!(),
        },
        "halt" => Ok(Instruction::Single([HALT])),
        _ => unreachable!(),
    }
}

/// # Panics
///
/// Panics if the arguments are not an immediate followed by a register or immediate.
/// Should be guarded by the calls to [`Instructions::matches`] in [`expand_macros`]
fn pull_double(mut args: Vec<Argument>, instruction: u8) -> Result<Instruction, Diagnostic> {
    let first: u8 = match args.swap_remove(0) {
        Argument::Reg(reg) => reg.inner as u8,
        _ => unreachable!(),
    };

    let second = match args.swap_remove(0) {
        Argument::Immediate(imm) => RegImm::Immediate(
            imm.value
                .try_into()
                .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
        ),
        Argument::Expr(expr) => {
            let imm = eval::eval_expr(expr)?;
            RegImm::Immediate(
                imm.value
                    .try_into()
                    .map_err(|_| spanned_error!(imm.span, "immediate out of range"))?,
            )
        }
        Argument::Reg(reg) => RegImm::Register(reg),
        _ => unreachable!(),
    };

    Ok(match second {
        RegImm::Register(reg) => Instruction::Double([instruction | first as u8, reg.inner as u8]),
        RegImm::Immediate(imm) => {
            Instruction::Double([instruction | IMMEDIATE_MASK | first as u8, imm])
        }
    })
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
enum Instruction {
    Single([u8; 1]),
    Double([u8; 2]),
    Triple([u8; 3]),
}

impl Deref for Instruction {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Instruction::Single(byte) => byte.as_slice(),
            Instruction::Double(bytes) => bytes.as_slice(),
            Instruction::Triple(bytes) => bytes.as_slice(),
        }
    }
}

fn compile(
    stream: Vec<CSeg>,
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

        for expr in segment.tokens {
            match expr {
                ExpTok::Instruction(inst) => {
                    let inst = match compile_instruction(inst, pc, &data, &labels) {
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

fn expand_macro(inst: Inst, macros: &HashMap<String, Macro>) -> Result<Vec<ExpTok>, Diagnostic> {
    let def = macros
        .get(&inst.name.value)
        .ok_or_else(|| spanned_error!(inst.name.span.clone(), "instruction not found with the given arguments"))?;
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

fn expand_macros(code: &mut Vec<CSeg>, macros: HashMap<String, Macro>) -> Result<(), Errors> {
    let mut errors = Errors::new();

    for segment in code {
        let mut position = 0;
        while let Some(expr) = segment.tokens.get(position) {
            if let ExpTok::Instruction(inst) = expr {
                if !INSTRUCTIONS.matches(&inst) {
                    match expand_macro(inst.clone(), &macros) {
                        Ok(expanded) => {
                            segment.tokens.splice(position..=position, expanded);
                        }
                        Err(err) => errors.push(err),
                    }
                }
            }
            position += 1;
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn assemble(mut ctx: ParseStream) -> Result<[u8; 1 << 16], Errors> {
    let data = assemble_data(ctx.data)?;
    expand_macros(&mut ctx.code, ctx.macros)?;
    compile(ctx.code, data)
}
