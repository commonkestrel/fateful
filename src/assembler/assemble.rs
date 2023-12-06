use super::{
    parse::{Context, Punctuated, Types, RegImm},
    token::{Register, Address},
    Errors,
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
    "push" => &[&[Types::REG.union(Types::IMM8)]],
    "pop" => &[&[Types::REG]],
    "jnz" => &[&[Types::REG.union(Types::IMM8)]],
    "halt" => &[&[]],
});

struct Instructions {
    matches: Map<&'static str, &'static [&'static [Types]]>,
}

impl Instructions {
    const fn new(matches: Map<&'static str, &'static [&'static [Types]]>) -> Self {
        Instructions { matches }
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
    LdAddr(Register, Address),
    LdHl(Register),
    StAddr(Address, Register),
    StHl(Register),
    Lda(Address),
    Push(RegImm),
    Pop(Register),
    Jnz(RegImm),
}

pub fn assemble(ctx: Context) -> Result<Vec<u8>, Errors> {
    println!("{ctx:#?}");
    todo!()
}
