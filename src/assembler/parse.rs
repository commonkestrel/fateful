//! WIP token tree parsing
//!
//! # Parsing Steps
//! 1. Expand if statements, defines, and parse macros
//! 2. Expand Macros
//! 3. Parse expressions and variables

use super::{
    diagnostic::Diagnostic,
    eval,
    lex::{self, PreProc, Register, Span, Token, TokenInner, TokenStream},
    token,
    token::{Ident, Immediate, NewLine},
    Errors,
};
use crate::{error, spanned_error, Token, assembler::lex::Punctuation};

use std::{collections::HashMap, iter, str::FromStr, sync::Arc};

use bitflags::bitflags;

pub fn parse(stream: TokenStream) -> Result<(), Errors> {
    let proc_stream = pre_proc(stream)?;

    Err(vec![error!("Parsing not yet implemented")])
}

struct Punctuated<T, S> {
    list: Vec<(T, S)>,
    last: Option<T>,
}

impl<T, S> Punctuated<T, S> {
    fn values(self) -> Vec<T> {
        match self.last {
            Some(last) => self.list.into_iter().map(|pair| pair.0).chain(iter::once(last)).collect(),
            None => self.list.into_iter().map(|pair| pair.0).collect(),
        }
    }
}

impl<T, S> Parsable for Punctuated<T, S>
where
    T: Parsable,
    S: Parsable,
{
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let mut list = Vec::new();
        let mut item = None;

        loop {
            match ctx.peek() {
                Some(Token{ span: _, inner: TokenInner::NewLine }) | None => break,
                _ => {},
            }

            match item {
                Some(it) => {
                    let seperator = ctx.parse()?;
                    list.push((it, seperator));
                    item = None;
                },
                None => {
                    let next = ctx.parse()?;
                    item = Some(next);
                }
            }
        }

        Ok(Self { list, last: item })
    }
}

enum Segment {
    CSeg(CSeg),
    DSeg(DSeg),
}

impl Segment {
    fn org<'a>(&'a mut self) -> &mut Option<Immediate> {
        match self {
            Segment::CSeg(cseg) => &mut cseg.org,
            Segment::DSeg(dseg) => &mut dseg.org,
        }
    }
}

struct PostProc {
    macros: Vec<Macro>,
    variables: HashMap<String, u16>,
}

pub struct DSeg {
    dseg: token::Dseg,
    org: Option<Immediate>,
    variables: HashMap<Ident, u16>,
}

impl DSeg {
    fn size(&self) -> Result<u16, Diagnostic> {
        let mut size: u16 = 0;

        for (name, var_size) in self.variables.iter() {
            size = size.checked_add(*var_size).ok_or_else(|| {
                spanned_error!(name.span.clone(), "data segment out of range")
                    .with_help("make sure your variables can be stored in less than 2^16 bytes.")
            })?;
        }

        Ok(size)
    }

    fn range(&self, ptr: u16) -> Result<std::ops::Range<u16>, Diagnostic> {
        let origin = match self.org {
            Some(ref imm) => imm.value.try_into().map_err(|_| spanned_error!(imm.span.clone(), "segment origin out of range"))?,
            None => ptr,
        };
        let top = origin + self.size()?;

        Ok(origin..top)
    }

    fn collect(all: Vec<Self>) -> Result<HashMap<Ident, u16>, Errors> {
        let mut errors = Errors::new();
        let mut ptr = 0x0000;

        let mut ranges = Vec::new();
        for dseg in all.iter() {
            let size = match dseg.size() {
                Ok(s) => s,
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            };
            let origin = match dseg.org {
                Some(ref imm) => match imm.value.try_into().map_err(|_| spanned_error!(imm.span.clone(), "segment origin out of range")) {
                    Ok(org) => org,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                },
                None => ptr,
            };
            let top = origin + size;

            ptr = top;
            ranges.push(origin..top);
        }

        for range in ranges {}

        todo!()
    }
}

pub struct CSeg {
    cseg: Option<token::Cseg>,
    org: Option<Immediate>,
    tokens: Vec<ExpTok>,
}

pub struct Context {
    code: Vec<CSeg>,
    data: Vec<DSeg>,
    current_segment: Segment,
    defines: HashMap<String, TokenStream>,
    macros: HashMap<String, Macro>,
    stream: TokenStream,
    position: usize,
}

impl Context {
    pub fn peek<'a>(&'a self) -> Option<&'a Token> {
        self.stream.get(self.position)
    }

    fn parse<R: Parsable>(&mut self) -> Result<R, Diagnostic> {
        R::parse(self)
    }
}

impl Iterator for Context {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.stream.get(self.position);
        self.position += 1;
        ret.cloned()
    }
}

enum ExpTok {
    Label(Label),
    Instruction(Inst),
}

impl Parsable for ExpTok {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let name: Ident = ctx.parse()?;

        if let Some(Token { span: _, inner: TokenInner::Punctuation(Punctuation::Colon) })  = ctx.peek() {
            let colon: Token![:] = ctx.parse()?;
            let nl: NewLine = ctx.parse()?;

            Ok(ExpTok::Label(Label { name, colon, nl }))
        } else {
            let args = ctx.parse()?;

            Ok(ExpTok::Instruction(Inst{ name, args }))
        }
    }
}

struct Inst {
    name: Ident,
    args: Punctuated<Token, Token![,]>,
}

struct Label {
    name: Ident,
    colon: Token![:],
    nl: NewLine,
}

fn pre_proc(mut stream: TokenStream) -> Result<(), Errors> {
    let mut ctx = Context {
        code: Vec::new(),
        data: Vec::new(),
        current_segment: Segment::CSeg(CSeg {
            org: None,
            cseg: None,
            tokens: Vec::new(),
        }),
        defines: HashMap::new(),
        macros: HashMap::new(),
        stream,
        position: 0,
    };

    let mut errors = Vec::new();

    loop {
        let tok = match ctx.peek() {
            Some(tok) => tok,
            None => break,
        }.to_owned();

        if let Err(err) = expand_preproc(tok, &mut ctx) {
            errors.push(err);
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    } else {
        Ok(())
    }
}

fn expand_preproc(peek: Token, ctx: &mut Context) -> Result<(), Diagnostic> {
    use TokenInner as TI;
    match peek.inner {
        TI::Ident(lex::Ident::PreProc(PreProc::Define)) => {
            let def: Define = ctx.parse()?;
            ctx.defines.insert(def.name, def.value);
        }
        TI::Ident(lex::Ident::PreProc(PreProc::If)) => eval_if(ctx)?,
        TI::Ident(lex::Ident::PreProc(PreProc::Org)) => {
            let origin: Org = ctx.parse()?;
            if ctx.current_segment.org().is_some() {
                return Err(
                    spanned_error!(origin.span, "Duplicate definitions of origin")
                        .with_help("`@org` can only be used once per section"),
                );
            } else {
                *ctx.current_segment.org() = Some(origin.address);
            }
        },
        TI::Ident(lex::Ident::PreProc(PreProc::Cseg)) => {
            let mut segment = Segment::CSeg(CSeg {
                cseg: Some(ctx.parse()?),
                org: None,
                tokens: Vec::new(),
            });

            std::mem::swap(&mut segment, &mut ctx.current_segment);

            match segment {
                Segment::CSeg(cseg) => ctx.code.push(cseg),
                Segment::DSeg(dseg) => ctx.data.push(dseg),
            }
        },
        TI::Ident(lex::Ident::PreProc(PreProc::Dseg)) => {
            let mut segment = Segment::DSeg(DSeg {
                dseg: ctx.parse()?,
                org: None,
                variables: HashMap::new(),
            });

            std::mem::swap(&mut segment, &mut ctx.current_segment);
            
            match segment {
                Segment::CSeg(cseg) => ctx.code.push(cseg),
                Segment::DSeg(dseg) => ctx.data.push(dseg),
            }
            
        }
        _ => {},
    }

    Ok(())
}

fn eval_if(ctx: &mut Context) -> Result<(), Diagnostic> {
    use TokenInner as TI;

    let start = ctx.position;
    let if_span = ctx
        .next()
        .ok_or_else(|| error!("`parse::eval_if` called without tokens").as_bug())?
        .span;

    let eval = if_expr(ctx)?;
    let mut depth = 0;
    let mut out = Vec::new();

    while let Some(tok) = ctx.peek() {
        match tok.inner {
            TI::Ident(lex::Ident::PreProc(PreProc::If))
            | TI::Ident(lex::Ident::PreProc(PreProc::IfDef)) => {
                _ = ctx.next();
                depth += 1;
            }
            TI::Ident(lex::Ident::PreProc(PreProc::EndIf)) => {
                _ = ctx.next();
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            }
            TI::Ident(lex::Ident::PreProc(PreProc::ElIf)) => {
                if depth == 0 {
                    if eval {
                        end_if(ctx, if_span)?;
                        break;
                    } else {
                        return eval_if(ctx);
                    }
                }
            }
            TI::Ident(lex::Ident::PreProc(PreProc::Else)) => {
                if depth == 0 {
                    if eval {
                        end_if(ctx, if_span)?;
                        break;
                    } else {
                        while let Some(more) = ctx.next() {
                            if matches!(more.inner, TI::Ident(lex::Ident::PreProc(PreProc::EndIf))) {
                            } else {
                            }
                        }
                        return Err(error!(""));
                    }
                }
            }
            _ => {
                if eval {
                    out.push(tok.clone());
                }
            }
        }
    }

    ctx.stream.splice(start..=ctx.position, out);
    ctx.position = start;

    Ok(())
}

fn if_expr(ctx: &mut Context) -> Result<bool, Diagnostic> {
    let start = ctx.position;

    let end = ctx
        .position(|tok| tok.inner == TokenInner::NewLine)
        .ok_or_else(|| {
            spanned_error!(
                ctx.stream[ctx.stream.len() - 1].span.clone(),
                "expected newline after `@if` expression, found `EOF`"
            )
        })?;

    if start == end {
        return Err(spanned_error!(
            ctx.stream[start].span.clone(),
            "expected expression, found newline"
        ));
    }

    let eval = eval::eval_no_paren(&ctx.stream[start..end], &ctx.defines)?;

    Ok(eval > 0)
}

fn end_if(ctx: &mut Context, err_span: Arc<Span>) -> Result<(), Diagnostic> {
    let mut depth = 0;

    while let Some(tok) = ctx.next() {
        use TokenInner as TI;
        match tok.inner {
            TI::Ident(lex::Ident::PreProc(PreProc::If)) => depth += 1,
            TI::Ident(lex::Ident::PreProc(PreProc::IfDef)) => depth += 1,
            TI::Ident(lex::Ident::PreProc(PreProc::IfNDef)) => depth += 1,
            TI::Ident(lex::Ident::PreProc(PreProc::EndIf)) => {
                depth -= 1;
                if depth == 0 {
                    return Ok(());
                }
            }
            _ => {},
        }
    }

    Err(spanned_error!(err_span, "unclosed if expression; expected `@endif`, found `eof`"))
}

pub trait Parsable: Sized {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Org {
    span: Arc<Span>,
    address: Immediate,
}

impl Parsable for Org {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let org: Token![@ORG] = ctx.parse()?;
        let addr: Immediate = ctx.parse()?;

        Ok(Org {
            span: Arc::new(Span {
                line: org.span.line,
                range: org.span.range.start..addr.span.range.end,
                source: org.span.source.clone(),
            }),
            address: addr,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub name: String,
    pub value: TokenStream,
}

impl Parsable for Define {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let _def: Token![@DEFINE] = ctx.parse()?;
        let name: Ident = ctx.parse()?;
        let assignment: TokenStream = ctx.parse()?;

        Ok(Define {
            name: name.value,
            value: assignment,
        })
    }
}

impl FromStr for Define {
    type Err = Diagnostic;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();

        let pos = match trimmed.find("=") {
            Some(pos) => pos,
            None => {
                return Ok(Define {
                    name: s.to_owned(),
                    value: Vec::new(),
                })
            }
        };

        let name = s[..pos].to_owned();
        if name.contains(' ') {
            return Err(spanned_error!(
                Span {
                    source: lex::Source::String(Arc::new(trimmed.to_owned())),
                    line: 0,
                    range: 0..trimmed.len(),
                }.into(),
                "define name cannot contain whitespace",
            ));
        }

        let lexed = lex::lex_string(&trimmed[(pos+1)..]);
        let mut value = match lexed {
            Ok(l) => l,
            Err(errors) => {
                for err in errors {
                    err.emit();
                }
                error!("Unable to lex define due to previous errors").scream();
            }
        };

        Ok(Define {name, value})
    }
}

pub enum Instruction {
    Add(Register, RegImm),
    Adc(Register, RegImm),
    Sub(Register, RegImm),
    Sbc(Register, RegImm),
    Nand(Register, RegImm),
    Or(Register, RegImm),
    Cmp(Register, RegImm),
    Mv(Register, RegImm),
    Ld(Register, Option<u16>),
    St(Register, Option<u16>),
    Lda(u16),
    Push(RegImm),
    Pop(Register),
    Jnz(Address),
    In(Register, RegImm),
    Out(RegImm, Register),
}

impl Instruction {
    pub fn len(&self) -> u16 {
        use Instruction as I;

        match self {
            I::Add(_, _) => 2,
            I::Adc(_, _) => 2,
            I::Sub(_, _) => 2,
            I::Sbc(_, _) => 2,
            I::Nand(_, _) => 2,
            I::Or(_, _) => 2,
            I::Cmp(_, _) => 2,
            I::Mv(_, _) => 2,
            I::Ld(_, Some(_)) => 3,
            I::Ld(_, None) => 1,
            I::St(_, Some(_)) => 3,
            I::St(_, None) => 1,
            I::Lda(_) => 3,
            I::Push(_) => 2,
            I::Pop(_) => 1,
            I::Jnz(_) => 3,
            I::In(_, _) => 2,
            I::Out(_, _) => 2,
        }
    }
}

pub enum RegImm {
    Register(Register),
    Immediate(u8),
}

pub enum Address {
    Literal(u16),
    Label(Ident),
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Types: u8 {
        const REG = 0b0000_0001;
        const ADDR = 0b0000_0010;
        const LABEL = 0b0000_0100;
        const STR = 0b0000_1000;
        const IMM8 = 0b0001_0000;
        const IMM16 = 0b0011_0000;
        const IMM32 = 0b0111_0000;
        const IMM64 = 0b1111_0000;
    }
}

pub struct Parameter {
    types: Types,
    name: String,
}

impl Parameter {
    fn fits(&self, token: &Token) -> bool {
        use TokenInner as TI;
        match token.inner {
            TI::String(_) => self.types.contains(Types::STR),
            TI::Immediate(imm) => match imm.checked_ilog2() {
                None | Some(0..=7) => self.types.contains(Types::IMM8),
                Some(8..=15) => self.types.contains(Types::IMM16),
                Some(16..=31) => self.types.contains(Types::IMM32),
                Some(32..=63) => self.types.contains(Types::IMM64),
                _ => unreachable!(),
            },
            TI::Address(_) => self.types.contains(Types::ADDR),
            TI::Ident(lex::Ident::Register(_)) => self.types.contains(Types::REG),
            TI::Ident(_) => self.types.contains(Types::LABEL),
            _ => false,
        }
    }
}

pub struct MacroDef {
    parameters: Vec<Parameter>,
    tokens: TokenStream,
}

impl MacroDef {
    pub fn fits(&self, tokens: &[Token]) -> bool {
        if self.parameters.len() != tokens.len() {
            return false;
        }

        self.parameters
            .iter()
            .zip(tokens)
            .all(|(param, token)| param.fits(token))
    }

    /// Must make sure that the provided parameters match this rule with [`MacroDef::fits`]
    pub fn expand(&self, parameters: &[Token]) -> Result<TokenStream, Diagnostic> {
        let mut expanded = TokenStream::with_capacity(self.tokens.len());

        let parameters: HashMap<String, &Token> = HashMap::from_iter(
            self.parameters
                .iter()
                .map(|p| p.name.to_owned())
                .zip(parameters.into_iter()),
        );

        for token in self.tokens.iter() {
            expanded.push(match &token.inner {
                TokenInner::Ident(lex::Ident::MacroVariable(name)) => {
                    (*parameters.get(name).ok_or(spanned_error!(
                        token.span.clone(),
                        "macro variable `{name}` not found in scope",
                    ))?)
                    .clone()
                }
                _ => token.clone(),
            })
        }

        Ok(expanded)
    }
}

pub struct Macro {
    name: Ident,
    definitions: Vec<MacroDef>,
}

impl Macro {
    fn expand(self, span: Arc<Span>, parameters: Vec<Token>) -> Result<TokenStream, Diagnostic> {
        let rule = self
            .definitions
            .iter()
            .find(|def| def.fits(&parameters))
            .ok_or_else(|| spanned_error!(span, "no rules matched "))?;
        rule.expand(&parameters)
    }
}