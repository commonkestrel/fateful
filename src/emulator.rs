use std::{
    fmt,
    io::{Read, Write},
    str::FromStr,
    time::{Duration, SystemTime, SystemTimeError},
};

use async_std::{
    channel::{self, Receiver, Sender, TryRecvError},
    io,
    sync::RwLock,
};
use bitflags::bitflags;
use clap::Args;
use clio::Input;
use once_cell::sync::{Lazy, OnceCell};
use thiserror::Error;

const CTRL_ROM1: &[u8; 1 << 11] = include_bytes!("ctrl1.rom");
const CTRL_ROM2: &[u8; 1 << 11] = include_bytes!("ctrl2.rom");
const CTRL_ROM3: &[u8; 1 << 11] = include_bytes!("ctrl3.rom");

#[derive(Error, Debug)]
pub enum EmulatorError {
    #[error("unable to read provided input")]
    Input(std::io::Error),
    #[error("error in stdin channel")]
    StdIn,
    #[error("unable to print to stdout")]
    StdOut(std::io::Error),
    #[error("error getting time elapsed")]
    Time(#[from] SystemTimeError),
    #[error("global state already set")]
    OnceFull,
    #[error("global state not initialized yet")]
    OnceEmpty,
}

#[derive(Debug, Args)]
pub struct EmulatorArgs {
    /// Input program ROM
    #[clap(value_parser, default_value = "-")]
    input: Input,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct ControlWord: u32 {
        /// ALU Add
        const ADD = 1 << 0;
        /// ALU Subtract
        const SUB = 1 << 1;
        /// ALU Nand
        const NAND = 1 << 2;
        /// ALU Or
        const OR = 1 << 3;
        /// ALU Bus In
        const AI = 1 << 4;
        /// ALU Bus Out
        const AO = 1 << 5;
        /// Register Bank Bus In
        const RI = 1 << 9;
        /// Register Bank Bus Out
        const RO = 1 << 10;
        /// Stack Pointer Increment
        const SPI = 1 << 11;
        /// Stack Pointer Decrement
        const SPD = 1 << 12;
        /// Program Counter Increment
        const PCI = 1 << 13;
        /// Set Program Counter
        const SPC = 1 << 14;
        /// Load instruction
        const LI = 1 << 15;
        /// Load first byte
        const LF = 1 << 16;
        /// Load second byte
        const LS = 1 << 17;
    }
}

#[derive(Debug, Clone)]
struct Adc {
    primary: u8,
    secondary: u8,
}

impl Default for Adc {
    fn default() -> Self {
        Adc {
            primary: 0,
            secondary: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct RegBank {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: u8,
}

impl RegBank {
    fn get_reg(&self, n: u8) -> u8 {
        match n {
            0 => self.a,
            1 => self.b,
            2 => self.c,
            3 => self.d,
            4 => self.e,
            5 => self.h,
            6 => self.l,
            7 => self.f,
            _ => unreachable!(),
        }
    }

    fn set_reg(&mut self, n: u8, val: u8) {
        match n {
            0 => self.a = val,
            1 => self.b = val,
            2 => self.c = val,
            3 => self.d = val,
            4 => self.e = val,
            5 => self.h = val,
            6 => self.l = val,
            7 => self.f = val,
            _ => unreachable!(),
        }
    }
}

impl Default for RegBank {
    fn default() -> Self {
        RegBank {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            f: 0,
        }
    }
}

impl fmt::Display for RegBank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\
                REGISTER A: {:#010b}\n\
                REGISTER B: {:#010b}\n\
                REGISTER C: {:#010b}\n\
                REGISTER D: {:#010b}\n\
                REGISTER E: {:#010b}\n\
                REGISTER H: {:#010b}\n\
                REGISTER L: {:#010b}\n\
                REGISTER F: {:#010b}\n\
            ",
            self.a, self.b, self.c, self.d, self.e, self.h, self.l, self.f,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Instruction {
    Add,
    Sub,
    Adc,
    Sbc,
    Nand,
    Or,
    Cmp,
    Mv,
    Ld,
    St,
    Lda,
    Push,
    Pop,
    Jnz,
    In,
    Out,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    F,
}

#[derive(Debug, Clone)]
struct Control {
    instruction: Instruction,
    immediate: bool,
    register: Register,
    primary: u8,
    secondary: u8,
    clock: u8,
}

impl Default for Control {
    fn default() -> Self {
        // CPU starts up with 0x00 in all control registers
        Control {
            instruction: Instruction::Add,
            immediate: false,
            register: Register::A,
            primary: 0,
            secondary: 0,
            clock: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    pc: u16,
    sp: u16,
    ctrl: Control,
    adc: Adc,
    bus: u8,
    bank: RegBank,
    speed: Option<Duration>,
    quit: bool,
    ports: [u8; 1 << 8],
    mem: Box<[u8]>,
    program: Box<[u8]>,
}

impl State {
    async fn tick(&mut self) -> Result<(), EmulatorError> {
        if self.cw().contains(ControlWord::LI) {}

        self.ctrl.clock += 1;
        Ok(())
    }

    fn cw(&self) -> ControlWord {
        let index = ((self.ctrl.instruction as u8) << 5
            | (self.ctrl.immediate as u8) << 4
            | self.ctrl.clock) as usize;

        let low = CTRL_ROM1[index] as u32;
        let mid = CTRL_ROM2[index] as u32;
        let high = CTRL_ROM3[index] as u32;

        ControlWord::from_bits_retain(low | (mid << 8) | (high << 16))
    }
}

impl State {
    fn init(program: Box<[u8]>) -> Self {
        State {
            pc: 0,
            sp: 0xFFFF,
            ctrl: Control::default(),
            adc: Adc::default(),
            bus: 0,
            bank: RegBank::default(),
            speed: None,
            quit: false,
            ports: [0; 1 << 8],
            mem: vec![0; 1 << 16].into_boxed_slice(),
            program,
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\
                PROGRAM COUNTER: {:#04X}\n\
                BUS: {}\n\
                {}\
            ",
            self.pc, self.bus, self.bank,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
enum SingleCmd {
    Get,
    Peek,
    Read,
    Run,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
enum DoubleCmd {
    Set,
    Poke,
    Write,
}

static STATE: OnceCell<RwLock<State>> = OnceCell::new();

pub async fn emulate(mut args: EmulatorArgs) -> Result<(), EmulatorError> {
    let mut program: Box<[u8]> = vec![0; 1 << 16].into_boxed_slice();

    args.input
        .read(&mut program)
        .map_err(|err| EmulatorError::Input(err))?;

    STATE
        .set(RwLock::new(State::init(program)))
        .map_err(|_| EmulatorError::OnceFull)?;

    print!("> ");
    std::io::stdout()
        .flush()
        .map_err(|err| EmulatorError::StdOut(err))?;
    let stdin = spawn_stdin_channel();

    let mut prev = SystemTime::now();

    loop {
        match stdin.try_recv() {
            Ok(s) => handle_input(s).await?,
            Err(TryRecvError::Closed) => return Err(EmulatorError::StdIn),
            Err(TryRecvError::Empty) => {}
        }

        if STATE.wait().read().await.quit {
            break;
        }

        if let Some(speed) = STATE
            .get()
            .ok_or(EmulatorError::OnceEmpty)?
            .read()
            .await
            .speed
        {
            if prev.elapsed()? >= speed {
                prev = SystemTime::now();
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .write()
                    .await
                    .tick()
                    .await?;
            } else {
                prev = SystemTime::now();
            }
        }
    }

    Ok(())
}

async fn handle_input(input: String) -> Result<(), EmulatorError> {
    match input.trim().split_once(' ') {
        Some((cmd, args)) => match cmd {
            "GET" => single_arg(SingleCmd::Get, args).await?,
            "SET" => double_arg(DoubleCmd::Set, args).await?,
            "PEEK" => single_arg(SingleCmd::Peek, args).await?,
            "POKE" => double_arg(DoubleCmd::Poke, args).await?,
            "READ" => single_arg(SingleCmd::Read, args).await?,
            "WRITE" => double_arg(DoubleCmd::Write, args).await?,
            "RUN" => single_arg(SingleCmd::Run, args).await?,
            _ => eprintln!("UNRECOGNIZED COMMAND: {cmd}"),
        },
        None => match input.trim() {
            "DUMP" => print!(
                "{}",
                STATE.get().ok_or(EmulatorError::OnceEmpty)?.read().await
            ),
            "QUIT" => {
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .write()
                    .await
                    .quit = true;
                return Ok(());
            }
            "STEP" => {
                if STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .read()
                    .await
                    .speed
                    .is_none()
                {
                    STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .write()
                        .await
                        .tick()
                        .await?
                } else {
                    eprintln!("INVALID COMMAND: STEP can only be used if the CPU is stopped")
                }
            }
            "HELP" => help(),
            "STOP" => {
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .write()
                    .await
                    .speed = None
            }
            "" => {}
            cmd => eprintln!("UNRECOGNIZED COMMAND: {cmd}"),
        },
    };

    print!("> ");
    std::io::stdout()
        .flush()
        .map_err(|err| EmulatorError::StdOut(err))?;

    Ok(())
}

fn help() {
    println!(
        "\
        SET <reg>, <val>    : Sets the value in the register `reg` to `val`\n\
        GET <reg>           : Gets the value in the register `reg`\n\
        PEEK <addr>         : Gets the value at the memory address `addr`\n\
        POKE <addr>, <val>  : Sets the value at the memory address `addr` to `val`\n\
        READ <port>         : Gets the value on the specified port `port`\n\
        WRITE <port>, <val> : Writes the value `val` to the specified port `port`\n\
        RUN <speed>         : Starts running the CPU at the specified `speed` (in hertz)\n\
        DUMP                : Dumps the current machine state\n\
        STEP                : Pulses the clock a single time (only available if the CPU is stopped)\n\
        STOP                : Stops the CPU clock\n\
        QUIT                : Quits the program\n\
        HELP                : Prints this message\
    "
    );
}

async fn single_arg(cmd: SingleCmd, arg: &str) -> Result<(), EmulatorError> {
    match cmd {
        SingleCmd::Get => {
            let reg = match parse_u8(arg.trim()) {
                Ok(reg) => reg,
                Err(_) => match arg.trim() {
                    "A" => 0,
                    "B" => 1,
                    "C" => 2,
                    "D" => 3,
                    "E" => 4,
                    "H" => 5,
                    "L" => 6,
                    "F" => 7,
                    _ => {
                        eprintln!("INVALID ARGUMENT: unable to parse register");
                        return Ok(());
                    }
                },
            };
            if reg >= 8 {
                eprintln!("INVALID ARGUMENT: register out of range");
                return Ok(());
            }
            println!(
                "REGISTER {reg}: {:#04X}",
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .read()
                    .await
                    .bank
                    .get_reg(reg)
            )
        }
        SingleCmd::Read => {
            let port = match parse_u8(arg.trim()) {
                Ok(reg) => reg,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse register number");
                    return Ok(());
                }
            };

            println!(
                "PORT {port}: {:#04X}",
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .read()
                    .await
                    .ports[port as usize]
            );
        }
        SingleCmd::Peek => {
            let addr = match parse_u16(arg.trim()) {
                Ok(addr) => addr,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse address");
                    return Ok(());
                }
            };

            println!(
                "{addr:#06X}: {:#04X}",
                STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .read()
                    .await
                    .mem[addr as usize]
            );
        }
        SingleCmd::Run => {
            let speed = match parse_u32(arg.trim()) {
                Ok(speed) => speed,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse speed");
                    return Ok(());
                }
            };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .speed = Some(Duration::from_secs(1) / speed);
        }
    }

    Ok(())
}

async fn double_arg(cmd: DoubleCmd, args: &str) -> Result<(), EmulatorError> {
    let (arg1, arg2) = match args.trim().split_once(',') {
        Some(args) => args,
        None => {
            eprintln!("INVALID ARGUMENT: expected two arguments, found one");
            return Ok(());
        }
    };

    match cmd {
        DoubleCmd::Set => {
            let reg = match parse_u8(arg1.trim()) {
                Ok(reg) => reg,
                Err(_) => match arg1.trim() {
                    "A" => 0,
                    "B" => 1,
                    "C" => 2,
                    "D" => 3,
                    "E" => 4,
                    "H" => 5,
                    "L" => 6,
                    "F" => 7,
                    _ => {
                        eprintln!("INVALID ARGUMENT: unable to parse register");
                        return Ok(());
                    }
                },
            };
            if reg >= 8 {
                eprintln!("INVALID ARGUMENT: register out of range");
                return Ok(());
            }

            let value = match parse_u8(arg2.trim()) {
                Ok(val) => val,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse value");
                    return Ok(());
                }
            };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .bank
                .set_reg(reg, value);
        }
        DoubleCmd::Poke => {
            let addr = match parse_u16(arg1.trim()) {
                Ok(addr) => addr,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse address");
                    return Ok(());
                }
            };

            let value = match parse_u8(arg2.trim()) {
                Ok(val) => val,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse value");
                    return Ok(());
                }
            };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .mem[addr as usize] = value;
        }
        DoubleCmd::Write => {
            let port = match parse_u8(arg1.trim()) {
                Ok(reg) => reg,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse register number");
                    return Ok(());
                }
            };

            let value = match parse_u8(arg2.trim()) {
                Ok(val) => val,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse value");
                    return Ok(());
                }
            };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .ports[port as usize] = value;
        }
    }

    Ok(())
}

fn parse_u8(int: &str) -> Result<u8, <u8 as FromStr>::Err> {
    if int.starts_with("0b") {
        u8::from_str_radix(&int[2..], 2)
    } else if int.starts_with("0o") {
        u8::from_str_radix(&int[2..], 8)
    } else if int.starts_with("0x") {
        u8::from_str_radix(&int[2..], 16)
    } else {
        u8::from_str_radix(int, 10)
    }
}

fn parse_u16(int: &str) -> Result<u16, <u16 as FromStr>::Err> {
    if int.starts_with("0b") {
        u16::from_str_radix(&int[2..], 2)
    } else if int.starts_with("0o") {
        u16::from_str_radix(&int[2..], 8)
    } else if int.starts_with("0x") {
        u16::from_str_radix(&int[2..], 16)
    } else {
        u16::from_str_radix(int, 10)
    }
}

fn parse_u32(int: &str) -> Result<u32, <u32 as FromStr>::Err> {
    if int.starts_with("0b") {
        u32::from_str_radix(&int[2..], 2)
    } else if int.starts_with("0o") {
        u32::from_str_radix(&int[2..], 8)
    } else if int.starts_with("0x") {
        u32::from_str_radix(&int[2..], 16)
    } else {
        u32::from_str_radix(int, 10)
    }
}

fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = channel::unbounded();
    async_std::task::spawn(watch_input(tx));
    rx
}

async fn watch_input(tx: Sender<String>) {
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).await.unwrap();
        tx.send(buffer).await.unwrap();
    }
}
