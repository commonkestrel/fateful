use std::sync::Arc;

use crate::{
    assembler::{
        diagnostic::Reference,
        lex::{Ident, Span, Token, TokenInner},
        parse::DSeg,
        token::Immediate,
    },
    note, spanned_error,
};

use super::{
    parse::{Address, Argument, CSeg, ExpTok, Inst, Macro, ParseStream, Types},
    token::Register,
    Diagnostic, Errors,
};

use std::collections::HashMap;

use phf::{phf_map, Map};

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
    "lda" => &[&[Types::ADDR]],
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

fn compile(
    inst: Inst,
    pc: u16,
    data: &HashMap<String, u16>,
    labels: &HashMap<String, u16>,
) -> Result<Vec<u8>, Diagnostic> {
    let arguments = inst.args.into_values();
    for ref mut arg in arguments {
        match arg {
            Argument::Addr(addr) => {
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
                        tok.inner = TokenInner::Immediate(*label as i128);
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
                        tok.inner = TokenInner::Immediate(*variable as i128);
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
            }
            _ => {}
        }
    }

    todo!()
}

enum RegImm {
    Immediate(u8),
    Register(Register),
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
    LdAddr(Register, Address),
    LdHl(Register),
    StAddr(Address, Register),
    StHl(Register),
    Lda(Address),
    LpmAddr(Register, Address),
    LpmHl(Register),
    Push(RegImm),
    Pop(Register),
    Jnz(RegImm),
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
        .ok_or_else(|| spanned_error!(inst.name.span.clone(), "instruction not found in scope"))?;
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

pub fn assemble(mut ctx: ParseStream) -> Result<Vec<u8>, Errors> {
    note!("{:#?}", ctx.code).emit();
    note!("{:#?}", ctx.data).emit();

    let data = assemble_data(ctx.data)?;
    expand_macros(&mut ctx.code, ctx.macros)?;

    todo!()
}
