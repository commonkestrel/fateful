use crate::{Token, spanned_error};

use super::{
    parse::{self, Types, MemAddr, ProgAddr, Address, ParseStream, Argument, Parenthesized, Punctuated, Inst},
    token::Register,
    Errors, lex::Span,
};

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

    fn fits(&self, inst: Inst) -> Result<bool, Diagnostic> {
        if let Some(builtin) = INSTRUCTIONS.matches.get(&inst.name.value) {
            builtin.iter().any(|matches| {
                if matches.len() != inst.args.len() {
                    return false;
                }

                inst.args.values().zip(matches.iter()).all(|(argument, types)| {
                    match argument {
                        Argument::Addr(addr) => {
                            match addr.inner {
                                Address::Immediate(_) => 
                            }
                        },
                        Argument::Expr(expr) => 
                    }
                })

            })
        } else {
            false
        }
    }
}

fn compile(inst: Inst) -> Result<Vec<u8>, Diagnostic> {
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
    LdAddr(Register, MemAddr),
    LdHl(Register),
    StAddr(MemAddr, Register),
    StHl(Register),
    Lda(Address),
    LpmAddr(Register, ProgAddr),
    LpmHl(Register),
    Push(RegImm),
    Pop(Register),
    Jnz(RegImm),
}

pub fn assemble(ctx: ParseStream) -> Result<Vec<u8>, Errors> {
    println!("{ctx:#?}");
    todo!()
}
