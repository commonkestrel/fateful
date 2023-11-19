use std::{
    collections::HashMap,
    ffi::{c_char, CStr, CString},
    fmt,
    io::{Read, Write},
    str::FromStr,
    sync::OnceLock,
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
use libloading::{Library, Symbol};
use modular_bitfield::{bitfield, BitfieldSpecifier};
use thiserror::Error;

const CTRL_LOW: &[u8; 1 << 8] = include_bytes!(concat!(env!("OUT_DIR"), "/ctrl_low.rom"));
const CTRL_MID: &[u8; 1 << 8] = include_bytes!(concat!(env!("OUT_DIR"), "/ctrl_mid.rom"));
const CTRL_HIGH: &[u8; 1 << 8] = include_bytes!(concat!(env!("OUT_DIR"), "/ctrl_high.rom"));

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

// TODO: Figure out how to work `IN` and `OUT` into 24 bits
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct Flags: u8 {
        /// Zero flag
        const Z = 1 << 0;
        /// Carry flag
        const C = 1 << 1;
        /// Less than flag
        const L = 1 << 2;
        /// Equal flag
        const E = 1 << 3;
        /// Greater than flag
        const G = 1 << 4;
        /// Halt flag
        const H = 1 << 7;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RegBank {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
}

impl RegBank {
    fn get_reg(&self, n: u8) -> u8 {
        match n {
            0 => self.a,
            1 => self.b,
            2 => self.c,
            3 => self.d,
            4 => self.e,
            5 => self.f,
            6 => self.h,
            7 => self.l,
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
            5 => self.f = val,
            6 => self.h = val,
            7 => self.l = val,
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
            f: 0,
            h: 0,
            l: 0,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, BitfieldSpecifier)]
#[bits = 4]
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
}

impl From<u8> for Instruction {
    fn from(val: u8) -> Self {
        match val {
            0x0 => Instruction::Add,
            0x1 => Instruction::Sub,
            0x2 => Instruction::Adc,
            0x3 => Instruction::Sbc,
            0x4 => Instruction::Nand,
            0x5 => Instruction::Or,
            0x6 => Instruction::Cmp,
            0x7 => Instruction::Mv,
            0x8 => Instruction::Ld,
            0x9 => Instruction::St,
            0xA => Instruction::Lda,
            0xB => Instruction::Push,
            0xC => Instruction::Pop,
            0xD => Instruction::Jnz,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, BitfieldSpecifier)]
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

impl From<u8> for Register {
    fn from(val: u8) -> Self {
        match val {
            0 => Register::A,
            1 => Register::B,
            2 => Register::C,
            3 => Register::D,
            4 => Register::E,
            5 => Register::H,
            6 => Register::L,
            7 => Register::F,
            _ => unreachable!(),
        }
    }
}

#[bitfield]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct InstructionHeader {
    #[bits = 4]
    instruction: Instruction,
    immediate: bool,
    #[bits = 3]
    register: Register,
}

#[derive(Debug, Clone)]
struct Control {
    head: InstructionHeader,
    clock: u8,
}

impl Default for Control {
    fn default() -> Self {
        // CPU starts up with 0x00 in all control registers
        Control {
            head: InstructionHeader::new(),
            clock: 0,
        }
    }
}

#[derive(Debug)]
struct State {
    pc: u16,
    sp: u16,
    ctrl: Control,
    adc: Adc,
    bus: u8,
    bank: RegBank,
    speed: Option<(Duration, u32)>,
    quit: bool,
    mem: Box<[u8]>,
    program: Box<[u8]>,
    peripherals: HashMap<u8, Peripheral>,
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
            mem: vec![0; 1 << 16].into_boxed_slice(),
            program,
            peripherals: HashMap::new(),
        }
    }

    async fn tick(&mut self) -> Result<bool, EmulatorError> {
        for periph in self.peripherals.values() {
            unsafe {
                if let Ok(step) = periph.lib.get::<unsafe extern "C" fn(hz: u32)>(b"step") {
                    step(self.speed.map(|clock| clock.1).unwrap_or(0));
                }
            }
        }

        // rising edge

        if self.cw().contains(ControlWord::LI) {
            let byte = self.program[self.pc as usize];
            self.ctrl.head = InstructionHeader::from_bytes([byte]);
        }

        // falling edge
        if self.cw().contains(ControlWord::CR) {
            self.ctrl.clock = 0;
        } else {
            self.ctrl.clock = self.ctrl.clock.wrapping_add(1);
        }
        Ok(true)
    }

    fn cw(&self) -> ControlWord {
        let index = ((self.ctrl.head.instruction() as u8) << 5
            | (self.ctrl.head.immediate() as u8) << 4
            | self.ctrl.clock) as usize;

        let low = CTRL_LOW[index] as u32;
        let mid = CTRL_MID[index] as u32;
        let high = CTRL_HIGH[index] as u32;

        ControlWord::from_bits_retain(low | (mid << 8) | (high << 16))
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
                PERIPHERALS: {:#?}\n\
            ",
            self.pc,
            self.bus,
            self.bank,
            self.peripherals
                .values()
                .map(|periph| periph.name.to_owned())
                .collect::<Vec<String>>()
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
enum SingleCmd {
    Get,
    Peek,
    Run,
    Drop,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
enum DoubleCmd {
    Set,
    Poke,
    Load,
}

static STATE: OnceLock<RwLock<State>> = OnceLock::new();

#[derive(Debug)]
struct Peripheral {
    name: String,
    lib: Library,
}

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

        if STATE
            .get()
            .ok_or(EmulatorError::OnceEmpty)?
            .read()
            .await
            .quit
        {
            break;
        }

        if let Some(speed) = STATE
            .get()
            .ok_or(EmulatorError::OnceEmpty)?
            .read()
            .await
            .speed
        {
            if prev.elapsed()? >= speed.0 {
                prev = SystemTime::now();
                let halted = STATE
                    .get()
                    .ok_or(EmulatorError::OnceEmpty)?
                    .write()
                    .await
                    .tick()
                    .await?;

                if halted {
                    halt().await?;
                }
            } else {
                prev = SystemTime::now();
            }
        }
    }

    Ok(())
}

async fn halt() -> Result<(), EmulatorError> {
    STATE
        .get()
        .ok_or(EmulatorError::OnceEmpty)?
        .write()
        .await
        .speed = None;

    println!(
        "\n\
        INFO: CPU halt detected\
        INFO: stopping clock\
        > "
    );

    Ok(())
}

async fn handle_input(input: String) -> Result<(), EmulatorError> {
    match input.trim().split_once(' ') {
        Some((cmd, args)) => match cmd {
            "GET" => single_arg(SingleCmd::Get, args).await?,
            "SET" => double_arg(DoubleCmd::Set, args).await?,
            "PEEK" => single_arg(SingleCmd::Peek, args).await?,
            "POKE" => double_arg(DoubleCmd::Poke, args).await?,
            "LOAD" => double_arg(DoubleCmd::Load, args).await?,
            "DROP" => single_arg(SingleCmd::Drop, args).await?,
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
                    let halted = STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .write()
                        .await
                        .tick()
                        .await?;

                    if halted {
                        eprintln!("INVALID COMMAND: CPU is halted, the clock cannot be stepped")
                    }
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
        RUN <speed>         : Starts running the CPU at the specified `speed` (in hertz)\
\n                      If `speed` is zero, the emulator will run as fast as possible.\n\
        LOAD <path>, <port> : Loads the library at the given path as a peripheral.\
\n                      Read more about peripherals and their requirements in the README.
        DROP <port>         : Disconnects the peripheral on the given port, unloading the module.\n\
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
        SingleCmd::Peek => {
            let addr = match parse_u16(arg.trim()) {
                Ok(addr) => addr,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse address");
                    return Ok(());
                }
            };

            let data: u8 = match addr {
                0x0000..=0xFFBF => {
                    STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .read()
                        .await
                        .mem[addr as usize]
                }
                0xFFC0..=0xFFFF => {
                    match STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .read()
                        .await
                        .peripherals
                        .get(&((addr - 0xFFC0) as u8))
                    {
                        Some(periph) => unsafe {
                            let r: Result<Symbol<unsafe extern "C" fn() -> u8>, libloading::Error> =
                                periph.lib.get(b"read");
                            match r {
                                Ok(read) => read(),
                                Err(_) => {
                                    eprintln!("PERIPHERAL ERROR: peripheral `{}` does not contain `read()` method.", periph.name);
                                    return Ok(());
                                }
                            }
                        },
                        None => 0x00,
                    }
                }
            };

            println!("{addr:#06X}: {data:#04X}");
        }
        SingleCmd::Run => {
            let speed = match parse_u32(arg.trim()) {
                Ok(speed) => speed,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse speed");
                    return Ok(());
                }
            };

            let duration = if speed == 0 {
                Duration::ZERO
            } else {
                Duration::from_secs(1) / speed
            };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .speed = Some((duration, speed));
        }
        SingleCmd::Drop => {
            let port = match parse_u8(arg.trim()) {
                Ok(port) => port,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse port");
                    return Ok(());
                }
            };

            let prev = STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .peripherals
                .remove(&port);

            if prev.is_none() {
                println!("WARNING: no peripheral found on port {port:#04X}")
            }
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

            match addr {
                0x0000..=0xFFBF => {
                    STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .write()
                        .await
                        .mem[addr as usize] = value
                }
                0xFFC0..=0xFFFF => {
                    match STATE
                        .get()
                        .ok_or(EmulatorError::OnceEmpty)?
                        .read()
                        .await
                        .peripherals
                        .get(&((addr - 0xFFC0) as u8))
                    {
                        Some(periph) => unsafe {
                            let r: Result<Symbol<unsafe extern "C" fn(u8)>, libloading::Error> =
                                periph.lib.get(b"write");
                            match r {
                                Ok(write) => write(value),
                                Err(_) => {
                                    eprintln!("PERIPHERAL ERROR: peripheral `{}` does not contain `write()` method.", periph.name);
                                    return Ok(());
                                }
                            }
                        },
                        None => {}
                    }
                }
            };
        }
        DoubleCmd::Load => {
            let port = match parse_u8(arg2.trim()) {
                Ok(port) => port,
                Err(_) => {
                    eprintln!("INVALID ARGUMENT: unable to parse port");
                    return Ok(());
                }
            };

            if port >= 64 {
                eprintln!("INVALID ARGUMENT: port out of range (port must be withing 0-63)");
                return Ok(());
            }
            let path = parse_path(arg1);

            let (lib, name) = unsafe {
                let lib = match Library::new(path) {
                    Ok(lib) => lib,
                    Err(err) => {
                        eprintln!("Unable to load peripheral library: {err}");
                        return Ok(());
                    }
                };

                let name = match lib.get::<unsafe extern "C" fn() -> *const i8>(b"name") {
                    Ok(name) => CStr::from_ptr(name()).to_str().ok().map(|s| s.to_owned()),
                    Err(_) => None,
                }
                .unwrap_or(arg1.to_owned());

                if let Ok(init) = lib.get::<unsafe extern "C" fn()>(b"init") {
                    init();
                }

                (lib, name)
            };

            let periph = Peripheral { name, lib };

            STATE
                .get()
                .ok_or(EmulatorError::OnceEmpty)?
                .write()
                .await
                .peripherals
                .insert(port, periph);
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

fn parse_path(source: &str) -> String {
    source.trim_matches('"').trim_matches('\'').to_owned()
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
