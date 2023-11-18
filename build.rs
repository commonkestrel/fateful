// Didn't make actual good error reporting since I'm the only one who'll be debugging this.

use bitflags::bitflags;
use logos::{Lexer, Logos};
use std::ops::Range;
use std::path::Path;
use std::str::FromStr;
use std::{env, fs};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r";[^\n]*")]
#[logos(skip r"[ \r\t\f]+")]
enum Token {
    #[regex(r"[._a-zA-Z][_a-zA-Z0-9]*:?", |lex| lex.slice().to_owned())]
    Ident(String),
    #[token("|")]
    Pipe,
    #[token("\n")]
    Newline,
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    Shadow(#[from] shadow_rs::ShadowError),
    #[error("unable to locate output directory")]
    OutDir,
    #[error(transparent)]
    Fs(#[from] std::io::Error),
    #[error("Unknown instruction: {0}")]
    Instruction(String),
    #[error("Unknown section: {0}")]
    Section(String),
    #[error("Unknown flag: {0}")]
    UnknownFlag(String),
    #[error("Expected `|`, found {0}")]
    UnexpectedFlag(String),
    #[error("Expected newline after")]
    Newline,
    #[error("Expected flag, found `|`")]
    Pipe,
    #[error("Top level cycles not allowed")]
    Top,
    #[error("Unknown token encountered at index: {0:?}")]
    Lex(Range<usize>),
}

fn main() -> Result<(), Error> {
    shadow_rs::new()?;

    println!("cargo:rerun-if-changed=src/microcode.asm");
    println!("cargo:rerun-if-changed=build.rs");

    let file = fs::read_to_string("src/microcode.asm")?;
    let lex = Token::lexer(&file);
    let stream = Stream::parse(lex)?;
    let microcode = stream.stitch();
    let (ctrl_low, (ctrl_mid, ctrl_high)): (Vec<u8>, (Vec<u8>, Vec<u8>)) = microcode
        .into_iter()
        .map(|cw| {
            (
                (cw.bits() & 0xFF) as u8,
                (
                    ((cw.bits() >> 8) & 0xFF) as u8,
                    ((cw.bits() >> 16) & 0xFF) as u8,
                ),
            )
        })
        .unzip();

    let out_env = env::var_os("OUT_DIR").ok_or(Error::OutDir)?;
    let out_dir = Path::new(&out_env);
    fs::write(out_dir.join("ctrl_low.rom"), &ctrl_low)?;
    fs::write(out_dir.join("ctrl_mid.rom"), &ctrl_mid)?;
    fs::write(out_dir.join("ctrl_high.rom"), &ctrl_high)?;

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Instruction {
    Add = 0x0,
    Sub = 0x1,
    Adc = 0x2,
    Sbc = 0x3,
    Nand = 0x4,
    Or = 0x5,
    Cmp = 0x6,
    Mv = 0x7,
    Ld = 0x8,
    St = 0x9,
    Lda = 0xA,
    Push = 0xB,
    Pop = 0xC,
    Jnz = 0xD,
    In = 0xE,
    Out = 0xF,
}

#[derive(Debug, Clone)]
struct Sequence {
    start: Vec<ControlWord>,
    reg: Vec<ControlWord>,
    imm: Vec<ControlWord>,
    end: Vec<ControlWord>,
}

impl Sequence {
    fn empty() -> Self {
        Sequence {
            start: Vec::new(),
            reg: Vec::new(),
            imm: Vec::new(),
            end: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct Stream {
    instructions: HashMap<Instruction, Sequence>,
}

bitflags! {
    /// Representation of the CPU Control Word
    ///
    /// Find more in-depth explanations of flags in `Arch.md`.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct ControlWord: u32 {
        /// ALU opcode low
        const AOL = 1 << 0;
        /// ALU opcode middle
        const AOM = 1 << 1;
        /// ALU opcode high
        const AOH = 1 << 2;
        /// ALU active
        const AA = 1 << 3;
        /// ALU load primary
        const ALP = 1 << 4;
        /// ALU load secondary
        const ALS = 1 << 5;
        /// Register bank bus in
        const RI = 1 << 6;
        /// Register bank bus out
        const RO = 1 << 7;
        /// Instruction builtin register address
        const RBA = 1 << 8;
        /// Instruction primary register address
        const RPA = 1 << 9;
        /// Stack pointer increment
        const SPI = 1 << 10;
        /// Stack pointer decrement
        const SPD = 1 << 11;
        /// Clock reset
        const CR = 1 << 12;
        /// Program counter increment
        const PCI = 1 << 13;
        /// Set program counter
        const JNZ = 1 << 14;
        /// Load instruction
        const LI = 1 << 15;
        /// Program out
        const PO = 1 << 16;
        /// Swap Temp Register
        const ST = 1 << 17;
        /// Transfer HIGH/LOW
        const THL = 1 << 18;
        /// Load Address
        const LA = 1 << 19;
        /// Store Address
        const SA = 1 << 20;
        /// ADDRESS low in
        const AL = 1 << 21;
        /// ADDRESS high in
        const AH = 1 << 22;
        /// Load Stack Pointer
        const LSP = 1 << 23;
    }
}

impl FromStr for ControlWord {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "aol" => Ok(ControlWord::AOL),
            "aom" => Ok(ControlWord::AOM),
            "aoh" => Ok(ControlWord::AOH),
            "aa" => Ok(ControlWord::AA),
            "alp" => Ok(ControlWord::ALP),
            "als" => Ok(ControlWord::ALS),
            "ri" => Ok(ControlWord::RI),
            "ro" => Ok(ControlWord::RO),
            "rba" => Ok(ControlWord::RBA),
            "rpa" => Ok(ControlWord::RPA),
            "spi" => Ok(ControlWord::SPI),
            "spd" => Ok(ControlWord::SPD),
            "cr" => Ok(ControlWord::CR),
            "pci" => Ok(ControlWord::PCI),
            "jnz" => Ok(ControlWord::JNZ),
            "li" => Ok(ControlWord::LI),
            "po" => Ok(ControlWord::PO),
            "st" => Ok(ControlWord::ST),
            "thl" => Ok(ControlWord::THL),
            "la" => Ok(ControlWord::LA),
            "sa" => Ok(ControlWord::SA),
            "al" => Ok(ControlWord::AL),
            "ah" => Ok(ControlWord::AH),
            "lsp" => Ok(ControlWord::LSP),
            _ => Err(Error::UnknownFlag(s.to_owned())),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Section {
    Start,
    Reg,
    Imm,
    End,
}

impl Stream {
    fn parse(lex: Lexer<Token>) -> Result<Self, Error> {
        let mut instructions = HashMap::new();
        let mut current_instr = None;
        let mut newline = false;
        let mut pipe = false;
        let mut current_sequence = Sequence::empty();
        let mut current_section = Section::Start;
        let mut current_cw = None;



        for (token, span) in lex.spanned() {
            println!("{token:?}");
            match token {
                Ok(tok) => match tok {
                    Token::Ident(i) => {
                        if i.starts_with('.') && i.ends_with(':') {
                            if current_instr.is_some() && newline {
                                current_section = match i.as_str() {
                                    ".start:" => Section::Start,
                                    ".reg:" => Section::Reg,
                                    ".imm:" => Section::Imm,
                                    ".both:" | ".end:" => Section::End,
                                    _ => return Err(Error::Section(i)),
                                };
                            } else {
                                if current_instr.is_some() {
                                    return Err(Error::Newline);
                                } else {
                                    return Err(Error::Top);
                                }
                            }
                        } else if i.ends_with(':') {
                            if let Some(instr) = current_instr {
                                instructions.insert(instr, current_sequence);
                                current_sequence = Sequence::empty();
                            }

                            current_instr = Some(match i.as_str() {
                                "add:" => Instruction::Add,
                                "sub:" => Instruction::Sub,
                                "adc:" => Instruction::Adc,
                                "sbc:" => Instruction::Sbc,
                                "nand:" => Instruction::Nand,
                                "or:" => Instruction::Or,
                                "cmp:" => Instruction::Cmp,
                                "mv:" => Instruction::Mv,
                                "ld:" => Instruction::Ld,
                                "st:" => Instruction::St,
                                "lda:" => Instruction::Lda,
                                "push:" => Instruction::Push,
                                "pop:" => Instruction::Pop,
                                "jnz:" => Instruction::Jnz,
                                "in:" => Instruction::In,
                                "out:" => Instruction::Out,
                                _ => return Err(Error::Instruction(i)),
                            });
                            newline = false;
                            current_section = Section::Start;
                        } else {
                            match current_cw {
                                Some(ref mut cw) => if pipe {
                                    *cw |= ControlWord::from_str(&i)?;
                                    pipe = false;
                                } else {
                                    return Err(Error::UnexpectedFlag(i));
                                },
                                None => current_cw = Some(ControlWord::from_str(&i)?),
                            }
                        }
                    },
                    Token::Newline => {
                        if current_instr.is_some() {
                            if newline && !pipe {
                                if let Some(cw) = current_cw {
                                    match current_section {
                                        Section::Start => current_sequence.start.push(cw),
                                        Section::Reg => current_sequence.reg.push(cw),
                                        Section::Imm => current_sequence.imm.push(cw),
                                        Section::End => current_sequence.end.push(cw),
                                    }
                                    current_cw = None;
                                }
                            } else {
                                newline = true;
                            }
                        }
                    },
                    Token::Pipe => if current_cw.is_some() {
                        pipe = true
                    } else {
                        return Err(Error::Pipe);
                    },
                },
                Err(_) => return Err(Error::Lex(span)),
            }
        }

        if let Some(instr) = current_instr {
            if let Some(cw) = current_cw {
                match current_section {
                    Section::Start => current_sequence.start.push(cw),
                    Section::Reg => current_sequence.reg.push(cw),
                    Section::Imm => current_sequence.imm.push(cw),
                    Section::End => current_sequence.end.push(cw),
                }
            }

            instructions.insert(instr, current_sequence);
        }

        println!("{:?}", instructions);

        Ok(Stream {instructions})
    }

    fn stitch(self) -> [ControlWord; 1 << 8] {
        let mut ctrl = [ControlWord::empty(); 1 << 8];

        for (instr, seq) in self.instructions {
            let base = (instr as u8) << 4;
            let mut reg = base;
            let mut imm = base & 0b1000;

            for cw in seq.start {
                ctrl[reg as usize] = cw;
                reg += 1;

                ctrl[imm as usize] = cw;
                imm += 1;
            }

            for cw in seq.reg {
                ctrl[reg as usize] = cw;
                reg += 1;
            }

            for cw in seq.imm {
                ctrl[imm as usize] = cw;
                reg += 1;
            }

            for cw in seq.end {
                ctrl[reg as usize] = cw;
                reg += 1;

                ctrl[imm as usize] = cw;
                imm += 1;
            }
        }

        ctrl
    }
}
