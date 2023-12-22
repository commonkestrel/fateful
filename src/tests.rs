use clap::Args;
use clio::Input;

use crate::assembler::tests::{
    assemble,
    lex::{self, Token, TokenInner},
    parse,
};
use crate::emulator::test_emulate;
use crate::{diagnostic::Diagnostic, error, spanned_error};
use crate::{Verbosity, VERBOSITY};

use std::{
    io::{stdout, Write},
    num::ParseIntError,
    thread,
    time::Duration,
};

use colored::Colorize;

#[derive(Debug, Args)]
pub struct TestArgs {
    inputs: Vec<Input>,
    #[clap(short, long, default_value = "500ms")]
    timeout: humantime::Duration,
}

pub async fn test_all(args: TestArgs) -> Result<(), ()> {
    let mut handles = Vec::new();

    for input in args.inputs {
        let name = format!("{input}");

        handles.push((
            name.clone(),
            thread::spawn(move || {
                let mut output = Vec::new();
                let name = format!("{input}");

                match test_file(input, args.timeout.into(), &mut output) {
                    Ok(_) => {
                        println!("{name} - {}", "success".green());
                        Ok(())
                    }
                    Err(err) => {
                        writeln!(output, "{err}").unwrap();
                        println!("{name} - {}", "failure".red());
                        Err(output)
                    }
                }
            }),
        ));
    }

    for handle in handles {
        handle
            .1
            .join()
            .expect("one of the test threads panicked")
            .map_err(|err| {
                println!("\n---- {} stdout ----", handle.0);
                stdout().write(&err).unwrap();
                println!();

                ()
            })?;
    }

    Ok(())
}

#[inline]
fn emit_errors(errors: Vec<Diagnostic>, mut out: impl std::io::Write) -> Diagnostic {
    for err in errors {
        write!(out, "{err}").unwrap();
    }

    error!("unable to assemble due to previous errors")
}

#[inline]
fn bank_assert(bank: u8, expected: Option<u8>) -> Result<(), Diagnostic> {
    if let Some(reg) = expected {
        if bank == reg {
            Ok(())
        } else {
            Err(error!(
                "register side does not equal expected value: {bank} != {reg}"
            ))
        }
    } else {
        Ok(())
    }
}

fn parse_expected(input: &str) -> Result<u8, ParseIntError> {
    if let Some(expected) = input.strip_prefix("0b") {
        u8::from_str_radix(expected, 2)
    } else if let Some(expected) = input.strip_prefix("0o") {
        u8::from_str_radix(expected, 8)
    } else if let Some(expected) = input.strip_prefix("0x") {
        u8::from_str_radix(expected, 16)
    } else {
        u8::from_str_radix(input, 10)
    }
}

fn test_file(
    input: Input,
    timeout: Duration,
    mut out: impl std::io::Write,
) -> Result<(), Diagnostic> {
    VERBOSITY.get_or_init(|| Verbosity::Error);

    let mut a = None;
    let mut b = None;
    let mut c = None;
    let mut d = None;
    let mut e = None;
    let mut f = None;
    let mut h = None;
    let mut l = None;

    let lexed = lex::lex(input).map_err(|errors| emit_errors(errors, &mut out))?;
    let mut run = true;

    let mut skipped = lexed.iter().filter(|tok| tok.inner != TokenInner::NewLine);
    while let Some(Token {
        span,
        inner: TokenInner::Doc(docstr),
    }) = skipped.next()
    {
        let trimmed = docstr.trim();
        if let Some(val) = trimmed.strip_prefix("a:") {
            a = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("b:") {
            b = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("c:") {
            c = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("d:") {
            d = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("e:") {
            e = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("f:") {
            f = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("h:") {
            h = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if let Some(val) = trimmed.strip_prefix("l:") {
            l = Some(parse_expected(val.trim()).map_err(|err| {
                spanned_error!(span.clone(), "unable to parse 8-bit integer: {err}")
            })?);
        } else if trimmed == "no-run" {
            run = false;
        }
    }

    let parsed = parse::parse(lexed).map_err(|errors| emit_errors(errors, &mut out))?;
    let assembled = assemble::assemble(parsed).map_err(|errors| emit_errors(errors, &mut out))?;

    if run {
        let bank = test_emulate(assembled.into(), timeout)
            .map_err(|_| error!("emulator exceeded timeout"))?;

        bank_assert(bank.a, a)?;
        bank_assert(bank.b, b)?;
        bank_assert(bank.c, c)?;
        bank_assert(bank.d, d)?;
        bank_assert(bank.e, e)?;
        bank_assert(bank.f, f)?;
        bank_assert(bank.h, h)?;
        bank_assert(bank.l, l)?;
    }

    Ok(())
}

#[test]
fn std() {
    if let Err(err) = test_file(
        Input::new("tests/std.asm").unwrap(),
        Duration::from_millis(250),
        stdout(),
    ) {
        err.scream();
    }
}

#[test]
fn fib() {
    if let Err(err) = test_file(
        Input::new("tests/fib.asm").unwrap(),
        Duration::from_millis(250),
        stdout(),
    ) {
        err.scream();
    }
}

#[test]
fn mem() {
    if let Err(err) = test_file(
        Input::new("tests/mem.asm").unwrap(),
        Duration::from_millis(250),
        stdout(),
    ) {
        err.scream()
    }
}

#[test]
fn comments() {
    if let Err(err) = test_file(
        Input::new("tests/comments.asm").unwrap(),
        Duration::from_millis(250),
        stdout(),
    ) {
        err.scream()
    }
}

#[test]
#[should_panic]
fn timeout() {
    if let Err(err) = test_file(
        Input::new("tests/timeout.asm").unwrap(),
        Duration::from_millis(250),
        stdout(),
    ) {
        err.scream()
    }
}
