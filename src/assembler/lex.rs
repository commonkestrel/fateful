use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

use super::ascii::{unescape_str, AsciiStr, UnescapeError};
use super::diagnostic::Diagnostic;
use super::Errors;
use crate::error;
use logos::{Lexer, Logos};

pub type TokenStream = Vec<Token>;
pub type LexResult = std::result::Result<TokenStream, Errors>;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub inner: TokenInner,
    pub span: Arc<Span>,
}

impl Token {
    const fn description(&self) -> &'static str {
        use TokenInner as TI;
        match self.inner {
            TI::Address(_) => "address",
            TI::Char(_) => "character",
            TI::Delimeter(delim) => delim.description(),
            TI::Doc(_) => "doc string",
            TI::Ident(ident) => ident.description(),
            TI::Immediate(_) => "immediate",
            TI::String(_) => "string",
            TI::NewLine => "newline",
            TI::Punctuation(punct) => punct.description(),
            TI::Location => "location",
        }
    }
}

impl FromStr for Token {
    type Err = Diagnostic;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let skipped = s.replace("\\\n", "");
        let mut lex = TokenInner::lexer(&skipped).spanned();
        let (token, span) = lex
            .next()
            .ok_or_else(|| Diagnostic::error("No tokens found in string"))?;
        let span = Arc::new(Span {
            line: 0,
            range: span,
            source: Source::String(Arc::new(s.to_owned())),
        });
        match token {
            Ok(inner) => Ok(Token { inner, span }),
            Err(mut err) => {
                err.set_span(span);
                Err(err)
            }
        }
    }
}

pub fn lex<P: AsRef<Path>>(path: P) -> LexResult {
    let mut tokens: TokenStream = Vec::new();
    let mut errs: Errors = Vec::new();
    let source = Arc::new(path.as_ref().to_path_buf());

    let mut prev_lines = String::new();
    let mut multiline = false;

    match File::open(&path) {
        Ok(file) => {
            for (line_num, line) in BufReader::new(file).lines().enumerate() {
                match line {
                    Ok(line) => {
                        // Treat multiple lines with `\` between them as one line
                        if !multiline {
                            prev_lines.clear();
                        }
                        if line.ends_with('\\') {
                            prev_lines += &line[..line.len() - 1];
                            multiline = true;
                            continue;
                        } else {
                            multiline = false;
                        }

                        for (token, span) in
                            TokenInner::lexer(&(prev_lines.clone() + &line)).spanned()
                        {
                            let spanned =
                                span.start + prev_lines.len()..span.end + prev_lines.len();
                            let span = Arc::new(Span {
                                line: line_num,
                                range: spanned,
                                source: Source::File(source.clone()),
                            });
                            match token {
                                Ok(tok) => tokens.push(Token { inner: tok, span }),
                                Err(mut err) => {
                                    err.set_span(span);
                                    errs.push(err);
                                }
                            }
                        }

                        tokens.push(Token {
                            inner: TokenInner::NewLine,
                            span: Arc::new(Span {
                                line: line_num,
                                range: 0..0,
                                source: Source::File(source.clone()),
                            }),
                        })
                    }
                    Err(err) => {
                        errs.push(Diagnostic::error(match err.kind() {
                            ErrorKind::InvalidData => format!("encountered invalid data on line {line_num} (likely not valid UTF-8)"),
                            _ => format!("encountered an unexpected error while reading the input file on line {line_num}: {}", err.kind()),
                        }));
                        break;
                    }
                }
            }
        }
        Err(err) => errs.push(Diagnostic::error(match err.kind() {
            ErrorKind::NotFound => format!("unable to locate file: {}", path.as_ref().display()),
            ErrorKind::PermissionDenied => "insufficient permissions".to_string(),
            _ => format!(
                "encountered an unexpected error while opening input file: {}",
                err.kind()
            ),
        })),
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

pub fn lex_string<S>(file: S) -> LexResult
where
    S: Into<String>,
{
    let mut tokens: TokenStream = Vec::new();
    let mut errs: Errors = Vec::new();
    let source = Arc::new(file.into());

    let mut prev_lines = String::new();
    let mut multiline = false;

    for (line_num, line) in source.lines().enumerate() {
        // Treat multiple lines with `\` between them as one line
        if !multiline {
            prev_lines.clear();
        }
        if line.ends_with('\\') {
            prev_lines += &line[..line.len() - 1];
            multiline = true;
            continue;
        } else {
            multiline = false;
        }

        for (token, span) in TokenInner::lexer(&(prev_lines.clone() + &line)).spanned() {
            let spanned = span.start + prev_lines.len()..span.end + prev_lines.len();
            let span = Arc::new(Span {
                line: line_num,
                range: spanned,
                source: Source::String(source.clone()),
            });
            match token {
                Ok(tok) => tokens.push(Token { inner: tok, span }),
                Err(mut err) => {
                    err.set_span(span);
                    errs.push(err);
                }
            }
        }
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = Diagnostic)]
#[logos(skip r"(//|;)[^\n]*")]
#[logos(skip r"[ \t\f]")]
pub enum TokenInner {
    #[regex(r"0b[01][_01]*", TokenInner::binary)]
    #[regex(r"0o[0-7][_0-7]*", TokenInner::octal)]
    #[regex(r"-?[1-9][_0-9]*", TokenInner::decimal)]
    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*", TokenInner::hexadecimal)]
    Immediate(i128),

    #[regex(r#""((\\")|[\x00-\x21\x23-\x7F])*""#, TokenInner::string)]
    #[regex(r##"r#"((\\")|[\x00-\x21\x23-\x7F])*"#"##, TokenInner::raw_string)]
    String(AsciiStr),

    #[regex(r"'[\x00-\x7F]*'", TokenInner::char)]
    #[regex(r#"'\\[(\\)n"at0rbfv]'"#, TokenInner::char)]
    #[regex(r"'\\x[[:xdigit:]]{1,2}'", TokenInner::char)]
    Char(u8),

    #[regex(r"\[0b[01][_01]*\]", Address::addr_bin)]
    #[regex(r"\[0o[0-7][_0-7]*\]", Address::addr_oct)]
    #[regex(r"\[[0-9][_0-9]*\]", Address::addr_dec)]
    #[regex(r"\[0x[0-9a-fA-F][_0-9a-fA-F]*\]", Address::addr_hex)]
    #[regex(r"\[[._a-zA-Z][_a-zA-Z0-9]*\]", Address::label)]
    Address(Address),

    #[regex(r"[._a-zA-Z][_a-zA-Z0-9]*", Ident::any)]
    #[regex(r"@[_a-zA-Z][_a-zA-Z0-9]*", Ident::pre_proc)]
    #[regex(r"%[_a-zA-Z][_a-zA-Z0-9]*", Ident::macro_variable)]
    #[regex(r"\$[_a-zA-Z][_a-zA-Z0-9]*", Ident::variable)]
    #[regex(r"()", Ident::punc)]
    Ident(Ident),

    #[token("(", Delimeter::open_paren)]
    #[token(")", Delimeter::close_paren)]
    #[token("[", Delimeter::open_bracket)]
    #[token("]", Delimeter::close_bracket)]
    #[token("{", Delimeter::open_brace)]
    #[token("}", Delimeter::close_brace)]
    Delimeter(Delimeter),

    #[token("=", Punctuation::eq)]
    #[token("==", Punctuation::eq_eq)]
    #[token("!=", Punctuation::ne)]
    #[token("<", Punctuation::lt)]
    #[token("<=", Punctuation::le)]
    #[token(">", Punctuation::gt)]
    #[token(">=", Punctuation::ge)]
    #[token("&", Punctuation::and)]
    #[token("&&", Punctuation::and_and)]
    #[token("|", Punctuation::or)]
    #[token("||", Punctuation::or_or)]
    #[token("^", Punctuation::caret)]
    #[token("!", Punctuation::not)]
    #[token("~", Punctuation::not)]
    #[token("+", Punctuation::plus)]
    #[token("-", Punctuation::minus)]
    #[token("*", Punctuation::star)]
    #[token("/", Punctuation::slash)]
    #[token("<<", Punctuation::shl)]
    #[token(">>", Punctuation::shr)]
    #[token(",", Punctuation::comma)]
    #[token(":", Punctuation::colon)]
    Punctuation(Punctuation),

    #[regex(r"///[^\n]*", TokenInner::doc)]
    Doc(String),

    #[token("$")]
    Location,

    #[token("\n")]
    NewLine,
}

macro_rules! varient {
    ($($fn:ident -> $ty:ident::$varient:ident),* $(,)?) => {
        $(fn $fn(_: &mut Lexer<TokenInner>) -> $ty {
            $ty::$varient
        })*
    };
}

impl TokenInner {
    fn binary(lex: &mut Lexer<TokenInner>) -> Option<i128> {
        let slice = lex.slice().replace("_", "");
        i128::from_str_radix(&slice.strip_prefix("0b")?, 2).ok()
    }

    fn octal(lex: &mut Lexer<TokenInner>) -> Option<i128> {
        let slice = lex.slice().replace("_", "");
        i128::from_str_radix(&slice.strip_prefix("0o")?, 8).ok()
    }

    fn decimal(lex: &mut Lexer<TokenInner>) -> Option<i128> {
        let slice = lex.slice().replace("_", "");
        i128::from_str_radix(&slice, 10).ok()
    }

    fn hexadecimal(lex: &mut Lexer<TokenInner>) -> Option<i128> {
        let slice = lex.slice().replace("_", "");
        i128::from_str_radix(&slice.strip_prefix("0x")?, 16).ok()
    }

    fn string(lex: &mut Lexer<TokenInner>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("\"")
            .ok_or_else(|| Diagnostic::error("string not prefixed with `\"`"))?
            .strip_suffix("\"")
            .ok_or_else(|| Diagnostic::error("string not suffixed with `\"`"))?;

        Ok(unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched '\\' at string index {index}")
                }
            })
        })?)
    }

    fn raw_string(lex: &mut Lexer<TokenInner>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("r#\"")
            .ok_or_else(|| Diagnostic::error("string not prefixed with `r#\"`"))?
            .strip_suffix("#\"")
            .ok_or_else(|| Diagnostic::error("string not suffixed with `\"#`"))?;

        Ok(unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })?)
    }

    fn char(lex: &mut Lexer<TokenInner>) -> Result<u8, Diagnostic> {
        let slice = lex.slice();
        Self::char_from_str(slice)
    }

    fn char_from_str(s: &str) -> Result<u8, Diagnostic> {
        let inner = s
            .strip_prefix('\'')
            .ok_or_else(|| Diagnostic::error("char not prefixed with `'`"))?
            .strip_suffix('\'')
            .ok_or_else(|| Diagnostic::error("char not suffixed with `'`"))?;

        let escaped = unescape_str(inner).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })?;
        Ok(escaped[0])
    }

    fn path(lex: &mut Lexer<TokenInner>) -> Option<Box<PathBuf>> {
        let slice = lex.slice().strip_prefix("<")?.strip_suffix(">")?;
        Some(Box::new(PathBuf::try_from(slice).ok()?))
    }

    fn path_string(lex: &mut Lexer<TokenInner>) -> Option<Box<PathBuf>> {
        let slice = lex.slice().strip_prefix("<\"")?.strip_suffix("\">")?;
        Some(Box::new(PathBuf::try_from(slice).ok()?))
    }

    fn doc(lex: &mut Lexer<TokenInner>) -> Result<String, Diagnostic> {
        Ok(lex
            .slice()
            .strip_prefix("///")
            .ok_or_else(|| Diagnostic::error("doc comment does not start with `///`"))?
            .trim()
            .to_owned())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Address {
    Immediate(u16),
    Label(String),
}

impl Address {
    fn trim(lex: &mut Lexer<TokenInner>) -> Result<String, Diagnostic> {
        Ok(lex
            .slice()
            .strip_prefix("[0b")
            .ok_or_else(|| Diagnostic::error("binary address does not start with `[0b`"))?
            .strip_suffix("]")
            .ok_or_else(|| Diagnostic::error("binary address does not end with `]`"))?
            .replace("_", ""))
    }

    fn addr_bin(lex: &mut Lexer<TokenInner>) -> Result<Address, Diagnostic> {
        let slice = Address::trim(lex)?;

        Ok(Address::Immediate(u16::from_str_radix(&slice, 2).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })?))
    }

    fn addr_oct(lex: &mut Lexer<TokenInner>) -> Result<Address, Diagnostic> {
        let slice = Address::trim(lex)?;

        Ok(Address::Immediate(u16::from_str_radix(&slice, 8).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })?))
    }

    fn addr_dec(lex: &mut Lexer<TokenInner>) -> Result<Address, Diagnostic> {
        let slice = Address::trim(lex)?;

        Ok(Address::Immediate(u16::from_str_radix(&slice, 10).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })?))
    }

    fn addr_hex(lex: &mut Lexer<TokenInner>) -> Result<Address, Diagnostic> {
        let slice = Address::trim(lex)?;

        Ok(Address::Immediate(u16::from_str_radix(&slice, 16).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })?))
    }

    fn label(lex: &mut Lexer<TokenInner>) -> Result<Address, Diagnostic> {
        Ok(Address::Label(Address::trim(lex)?))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ident {
    Register(Register),
    PreProc(PreProc),
    Instruction(Instruction),
    Variable(String),
    MacroVariable(String),
    Ty(Ty),
    Ident(String),
}

impl Ident {
    const fn description(&self) -> &'static str {
        match self {
            Ident::Register(_) => "register",
            Ident::PreProc(pp) => pp.description(),
            Ident::Instruction(instr) => instr.description(),
            Ident::Variable(_) => "variable",
            Ident::MacroVariable(_) => "macro variable",
            Ident::Ty(_) => "type",
            Ident::Ident(_) => "identifier",
        }
    }

    fn variable(lex: &mut Lexer<TokenInner>) -> Result<Ident, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("$")
            .ok_or_else(|| Diagnostic::error("variable not prefixed by `$`"))?;
        Ok(Ident::Variable(slice.to_owned()))
    }

    fn macro_variable(lex: &mut Lexer<TokenInner>) -> Result<Ident, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("%")
            .ok_or_else(|| Diagnostic::error("macro variable not prefixed by `%`"))?;
        Ok(Ident::MacroVariable(slice.to_owned()))
    }

    fn pre_proc(lex: &mut Lexer<TokenInner>) -> Result<Ident, Diagnostic> {
        Ok(Ident::PreProc(PreProc::from_str(lex.slice())?))
    }

    fn any(lex: &mut Lexer<TokenInner>) -> Ident {
        let slice = lex.slice();

        if let Ok(register) = Register::from_str(slice) {
            Ident::Register(register)
        } else if let Ok(instruction) = Instruction::from_str(slice) {
            Ident::Instruction(instruction)
        } else if let Ok(ty) = Ty::from_str(slice) {
            Ident::Ty(ty)
        } else {
            Ident::Ident(slice.to_owned())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Register {
    /// GP register A.
    A,
    /// GP register B.
    B,
    /// GP register C.
    C,
    /// GP register D.
    D,
    /// GP register Z (disposable).
    Z,
    /// Status register
    S,
    /// Memory index low.
    L,
    /// Memory index high.
    H,
}

impl FromStr for Register {
    type Err = Diagnostic;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "r0" => Ok(Register::A),
            "r1" => Ok(Register::B),
            "r2" => Ok(Register::C),
            "r3" => Ok(Register::D),
            "r4" => Ok(Register::Z),
            "r5" => Ok(Register::S),
            "r6" => Ok(Register::L),
            "r7" => Ok(Register::H),
            _ => Err(error!("unknown register")),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PreProc {
    Include,
    Macro,
    Define,
    UnDef,
    IfDef,
    IfNDef,
    If,
    Else,
    ElIf,
    EndIf,
    Org,
    Cseg,
    Dseg,
    Byte,
    Double,
    Quad,
    Var,
}

impl PreProc {
    const fn description(&self) -> &'static str {
        use PreProc as PP;
        match self {
            PP::Include => "`@include`",
            PP::Macro => "`@macro`",
            PP::Define => "`@define`",
            PP::UnDef => "`@undef`",
            PP::IfDef => "`@ifdef`",
            PP::IfNDef => "`@ifndef`",
            PP::If => "`@if`",
            PP::Else => "`@else`",
            PP::ElIf => "`@elif`",
            PP::EndIf => "`@endif`",
            PP::Org => "`@org`",
            PP::Cseg => "`@cseg`",
            PP::Dseg => "`@dseg`",
            PP::Byte => "`@byte`",
            PP::Double => "`@double`",
            PP::Quad => "`@quad`",
            PP::Var => "`@var`",
        }
    }
}

impl FromStr for PreProc {
    type Err = Diagnostic;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let argument = s
            .strip_prefix("@")
            .ok_or_else(|| Diagnostic::error("Preprocessor argument not prefixed by `@`"))?;

        use PreProc as PP;

        match argument {
            "include" => Ok(PP::Include),
            "macro" => Ok(PP::Macro),
            "define" => Ok(PP::Define),
            "undef" => Ok(PP::UnDef),
            "ifdef" => Ok(PP::IfDef),
            "ifndef" => Ok(PP::IfNDef),
            "if" => Ok(PP::If),
            "else" => Ok(PP::Else),
            "elif" => Ok(PP::ElIf),
            "endif" => Ok(PP::EndIf),
            "org" => Ok(PP::Org),
            "cseg" => Ok(PP::Cseg),
            "dseg" => Ok(PP::Dseg),
            "byte" => Ok(PP::Byte),
            "double" => Ok(PP::Double),
            "quad" => Ok(PP::Quad),
            "var" => Ok(PP::Var),
            _ => Err(error!("Unrecognized preprocessor argument")),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Ty {
    Any,
    Reg,
    Addr,
    Label,
    Imm8,
    Imm16,
    Imm32,
    Imm64,
    Str,
}

impl FromStr for Ty {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "any" => Ok(Ty::Any),
            "reg" => Ok(Ty::Reg),
            "addr" => Ok(Ty::Addr),
            "label" => Ok(Ty::Label),
            "imm" | "imm8" => Ok(Ty::Imm8),
            "imm16" => Ok(Ty::Imm16),
            "imm32" => Ok(Ty::Imm32),
            "imm64" => Ok(Ty::Imm64),
            "str" => Ok(Ty::Str),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
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
    Halt,
}

impl Instruction {
    const fn description(&self) -> &'static str {
        use Instruction as I;
        match self {
            I::Add => "`add`",
            I::Sub => "`sub`",
            I::Adc => "`adc`",
            I::Sbc => "`sbc`",
            I::Nand => "`nand`",
            I::Or => "`or`",
            I::Cmp => "`cmp`",
            I::Mv => "`mv`",
            I::Ld => "`ld`",
            I::St => "`st`",
            I::Lda => "`lda`",
            I::Push => "`push`",
            I::Pop => "`pop`",
            I::Jnz => "`jnz`",
            I::Halt => "`halt`",
        }
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimeter {
    OpenParen,
    ClosedParen,
    OpenBracket,
    ClosedBracket,
    OpenBrace,
    ClosedBrace,
}

impl Delimeter {
    varient! {
        open_paren -> Delimeter::OpenParen,
        close_paren -> Delimeter::ClosedParen,
        open_bracket -> Delimeter::OpenBracket,
        close_bracket -> Delimeter::ClosedBracket,
        open_brace -> Delimeter::OpenBrace,
        close_brace -> Delimeter::ClosedBrace,
    }

    const fn description(&self) -> &'static str {
        use Delimeter as D;
        match self {
            D::OpenParen => "`(`",
            D::ClosedParen => "`)`",
            D::OpenBracket => "`[`",
            D::ClosedBracket => "`]`",
            D::OpenBrace => "`{`",
            D::ClosedBrace => "`}`",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    /// `=` (variable assignment)
    Eq,
    /// `=` (pre-proc eval: equality)
    EqEq,
    /// `!=` (pre-proc eval: not equal)
    Ne,
    /// `<` (pre-proc eval: less than)
    Lt,
    /// `<=` (pre-proc eval: less or equal)
    Le,
    /// `>` (pre-proc eval: greater than)
    Gt,
    /// `>=` (pre-proc eval: greater or equal)
    Ge,
    /// `&` (expression parsing: bitwise and)
    And,
    /// `&&` (pre-proc eval: and)
    AndAnd,
    /// `|` (expression parsing: bitwise or)
    Or,
    /// `||` (pre-proc eval: or)
    OrOr,
    /// `^` (expression parsing: bitwise xor)
    Caret,
    /// `!` or `~` (expression parsing: bitwise not)
    Not,
    /// `/` (expression parsing: division) (pre-proc eval: path seperator)
    Slash,
    /// `+` (expression parsing: addition)
    Plus,
    /// `-` (expression parsing: subtraction)
    Minus,
    /// `*` (expression parsing: multiplication)
    Star,
    /// `<<` (expression parsing: shift left)
    Shl,
    /// `>>` (expression parsing: shift right)
    Shr,
    /// `,` (argument seperator)
    Comma,
    /// `:` (label definition)
    Colon,
}

impl Punctuation {
    varient! {
        eq -> Punctuation::Eq,
        eq_eq -> Punctuation::EqEq,
        ne -> Punctuation::Ne,
        lt -> Punctuation::Lt,
        le -> Punctuation::Le,
        gt -> Punctuation::Gt,
        ge -> Punctuation::Ge,
        and -> Punctuation::And,
        and_and -> Punctuation::AndAnd,
        or -> Punctuation::Or,
        or_or -> Punctuation::OrOr,
        caret -> Punctuation::Caret,
        not -> Punctuation::Not,
        slash -> Punctuation::Slash,
        plus -> Punctuation::Plus,
        minus -> Punctuation::Minus,
        star -> Punctuation::Star,
        shl -> Punctuation::Shl,
        shr -> Punctuation::Shr,
        comma -> Punctuation::Comma,
        colon -> Punctuation::Colon,
    }

    const fn description(&self) -> &'static str {
        use Punctuation as P;
        match self {
            P::And => "`&`",
            P::AndAnd => "`&&`",
            P::Caret => "`^`",
            P::Colon => "`:`",
            P::Comma => "`,`",
            P::Eq => "`=`",
            P::EqEq => "`==`",
            P::Ge => "`>=`",
            P::Gt => "`>`",
            P::Le => "`<=`",
            P::Lt => "`<`",
            P::Minus => "`-`",
            P::Ne => "`!=`",
            P::Not => "`!` or `~`",
            P::Or => "`|`",
            P::OrOr => "`||`",
            P::Plus => "`+`",
            P::Shl => "`<<`",
            P::Shr => "`>>`",
            P::Slash => "/",
            P::Star => "*",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Source {
    File(Arc<PathBuf>),
    String(Arc<String>),
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::File(path) => write!(f, "{}", path.as_path().display()),
            Source::String(s) => {
                if s.contains('\n') {
                    Ok(())
                } else {
                    write!(f, "\"{s}\"")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub range: Range<usize>,
    pub source: Source,
}

impl Span {
    /// Line number for display.
    ///
    /// # Note
    /// Since this is for display, 1 is added to the line index.
    pub fn line_number(&self) -> usize {
        self.line + 1
    }

    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn line(&self) -> Result<String, Diagnostic> {
        match &self.source {
            Source::File(path) => {
                let file = File::open(path.as_ref()).map_err(|_| {
                    Diagnostic::error(format!("Unable to open file {}", path.as_path().display()))
                })?;
                let reader = BufReader::new(file);

                reader
                    .lines()
                    .nth(self.line)
                    .ok_or_else(|| {
                        Diagnostic::error("Line should be fully contained in the source file")
                    })?
                    .map_err(|_| {
                        Diagnostic::error(format!(
                            "Unable to read line {} from file {}",
                            self.line,
                            path.as_path().display()
                        ))
                    })
            }
            Source::String(s) => s
                .lines()
                .nth(self.line)
                .ok_or_else(|| {
                    Diagnostic::error("Line should be fully contained in the source string")
                })
                .map(|line| line.to_owned()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn delim() {
        let example = "< )".to_owned();

        let lexed = match lex_string(example) {
            Ok(tokens) => tokens,
            Err(errors) => {
                for error in errors {
                    error.force_emit();
                }
                Diagnostic::error("lexing failed due to previous errors").scream();
            }
        };

        println!(
            "{:?}",
            lexed
                .into_iter()
                .map(|tok| tok.inner)
                .collect::<Vec<TokenInner>>()
        );
    }
}
