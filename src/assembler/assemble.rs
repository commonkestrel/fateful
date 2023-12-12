

use crate::{assembler::{lex::{TokenInner, Ident, Token}, token::Immediate, diagnostic::Reference}, spanned_error};

use super::{
    Diagnostic,
    parse::{Types, Address, ParseStream, Argument, Inst},
    token::Register,
    Errors,
};

use std::collections::HashMap;

use phf::{phf_map, Map};

static INSTRUCTIONS: Instructions = Instructions::new(phf_map! {
    "add" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "sub" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "adc" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "sbc" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "nand" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "or" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "cmp" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "mv" => &[&[Types::REG, Types::REG.union(Types::IMM8)]],
    "ld" => &[&[Types::REG], &[Types::REG, Types::ADDR]],
    "st" => &[&[Types::REG], &[Types::ADDR, Types::REG]],
    "lda" => &[&[Types::ADDR]],
    "lpm" => &[&[Types::REG], &[Types::REG, Types::LABEL]],
    "push" => &[&[Types::REG.union(Types::IMM8)]],
    "pop" => &[&[Types::REG]],
    "jnz" => &[&[Types::REG.union(Types::IMM8)]],
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

    fn matches(&self, inst: Inst) -> bool {
        if let Some(builtin) = INSTRUCTIONS.matches.get(&inst.name.value) {
            builtin.iter().any(|matches| {
                if matches.len() != inst.args.len() {
                    return false;
                }

                inst.args.values().zip(matches.iter()).all(|(argument, types)| {
                    match argument {
                        Argument::Addr(_) => types.intersects(Types::LABEL | Types::ADDR),
                        Argument::Expr(_) | Argument::Immediate(_) => types.contains(Types::IMM8),
                        Argument::Reg(_) => types.contains(Types::REG),
                    }
                })

            })
        } else {
            false
        }
    }
}

fn compile(inst: Inst, pc: u16, data: &HashMap<String, u16>, labels: &HashMap<String, u16>) -> Result<Vec<u8>, Diagnostic> {
    let arguments = inst.args.into_values();
    for ref mut arg in arguments {
        match arg {
            Argument::Addr(addr) => {
                let mut mem = None;
                let mut prog = None;

                for tok in addr.inner.iter_mut() {
                    if let Token { span, inner: TokenInner::Ident(Ident::Ident(val)) } = tok {
                        if let Some(prev) = mem {
                            return Err(Diagnostic::referencing_error(span.clone(), "unexpected label in memory address", Reference::new(prev, "interpreted as memory address due to this reference")));
                        }
                        prog.get_or_insert(span.clone());

                        let label = labels.get(val).ok_or_else(|| spanned_error!(span.clone(), "unrecognized identifier in expression"))?;
                        tok.inner = TokenInner::Immediate(*label as i128);
                    } else if let Token { span, inner: TokenInner::Ident(Ident::Variable(var)) } = tok {
                        if let Some(prev) = prog {
                            return Err(Diagnostic::referencing_error(span.clone(), "unexpected variable in program address", Reference::new(prev, "interpreted as program address due to this reference")));
                        }
                        mem.get_or_insert(span.clone());

                        let variable = data.get(var).ok_or_else(|| spanned_error!(span.clone(), "variable not found in scope"))?;
                        tok.inner = TokenInner::Immediate(*variable as i128);
                    } else if let Token { span, inner: TokenInner::Location } = tok {
                        if let Some(prev) = mem {
                            return Err(Diagnostic::referencing_error(span.clone(), "unexpected program location in memory address", Reference::new(prev, "interpreted as memory address due to this reference")));
                        }
                        prog.get_or_insert(span.clone());

                        tok.inner = TokenInner::Immediate(pc as i128);
                    }
                }
            },
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

pub fn assemble_data(_stream: &ParseStream) -> Result<HashMap<String, u16>, Diagnostic> {
    todo!()
}

pub fn assemble(_ctx: ParseStream) -> Result<Vec<u8>, Errors> {
    
    
    todo!()
}
