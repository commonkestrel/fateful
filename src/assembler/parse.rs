//! WIP token tree parsing
//!
//! # Parsing Steps
//! 1. Expand if statements, defines, and parse macros
//! 2. Expand Macros
//! 3. Parse expressions and variables

use super::{
    diagnostic::{Diagnostic, Reference},
    eval, include,
    lex::{self, Delimeter, PreProc, Punctuation, Span, Token, TokenInner, TokenStream},
    token::{self, ClosedBracket, OpenBracket, Variable, Register},
    token::{
        ClosedBrace, ClosedParen, Ident, Immediate, LitString, MacroVariable, NewLine, OpenBrace,
        OpenParen, Ty,
    },
    Errors,
};
use crate::{error, note, spanned_error, Token};

use std::{collections::HashMap, iter, str::FromStr, sync::Arc, slice, option};

use bitflags::bitflags;

#[derive(Debug)]
pub struct Punctuated<T, S> {
    list: Vec<(T, S)>,
    last: Option<T>,
}

impl<T, S> Punctuated<T, S> {
    pub fn first<'a>(&'a self) -> Option<&T> {
        self.last.as_ref().and(self.list.last().map(|last| &last.0))
    }

    pub fn len(&self) -> usize {
        self.list.len() + if self.last.is_some() { 1 } else { 0 }
    }

    pub fn values(&self) -> Box<dyn Iterator<Item=&T>> {
        Box::new(self.list.iter().map(|pair| &pair.0).chain(self.last.iter()))
    }

    pub fn into_values(self) -> Vec<T> {
        match self.last {
            Some(last) => self
                .list
                .into_iter()
                .map(|pair| pair.0)
                .chain(iter::once(last))
                .collect(),
            None => self.list.into_iter().map(|pair| pair.0).collect(),
        }
    }
}

macro_rules! punctuated {
    ($ctx:expr) => {
        punctuated!($ctx, $crate::assembler::lex::TokenInner::NewLine)
    };
    ($ctx:expr, $end:pat) => {{
        let mut list = Vec::new();
        let mut item = None;

        loop {
            match $ctx.peek() {
                Some(Token {
                    span: _,
                    inner: $end,
                })
                | None => break,
                _ => {}
            }

            match item {
                Some(it) => {
                    let seperator = $ctx.parse()?;
                    list.push((it, seperator));
                    item = None;
                }
                None => {
                    let next = $ctx.parse()?;
                    item = Some(next);
                }
            }
        }

        Ok(Punctuated { list, last: item })
    }};
}

macro_rules! wrapped {
    ($name:ident, $macro:ident, $open_token:pat => $open:ident, $close_token:pat => $close:ident, $closing:literal $(,)?) => {
        #[derive(Debug)]
        pub struct $name<T> {
            pub open: $open,
            pub inner: T,
            pub close: $close,
        }

        macro_rules! $macro {
            ($ctx:expr) => {(|| {
                let open: $open = $ctx.parse()?;
                let mut tokens = TokenStream::new();
                let mut depth = 1;

                while let Some(tok) = $ctx.peek() {
                    match tok.inner {
                        $open_token => depth += 1,
                        $close_token => {
                            depth -= 1;
                            if depth == 0 {
                                let close: $close = $ctx.parse()?;
                                return Ok($name {
                                    open,
                                    inner: tokens,
                                    close,
                                });
                            }
                        }
                        _ => {}
                    }

                    // we know `next()` will return `Some()` since `peek()` was `Some()`
                    tokens.push(unsafe { $ctx.next().unwrap_unchecked() });
                }

                Err(spanned_error!(
                    open.span,
                    concat!("unclosed delimeter; expected closing `", $closing, "`")
                ))
            })()};
            ($ctx:expr, $parsable:ty) => {{
                let open: $open = $ctx.parse()?;
                let inner: $parsable = $ctx.parse()?;
                let close = match $ctx.next() {
                    Some(Token { span, inner: $close_token }) => $close { span },
                    Some(tok) => return Err(spanned_error!(
                        tok.span,
                        "",
                    )),
                    _ => return Err(spanned_error!(
                        open.span,
                        concat!("unclosed delimeter; expected closing `", $closing, "`")
                    )),
                };

                Ok($name {
                    open,
                    inner,
                    close,
                })
            }}
        }

        impl<T> std::ops::Deref for $name<T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}

wrapped!(
    Braced,
    braced,
    TokenInner::Delimeter(Delimeter::OpenBrace) => OpenBrace,
    TokenInner::Delimeter(Delimeter::ClosedBrace) => ClosedBrace,
    "}}"
);

wrapped!(
    Parenthesized,
    parenthesized,
    TokenInner::Delimeter(Delimeter::OpenParen) => OpenParen,
    TokenInner::Delimeter(Delimeter::ClosedParen) => ClosedParen,
    ")",
);

wrapped!(
    Bracketed,
    bracketed,
    TokenInner::Delimeter(Delimeter::OpenBracket) => OpenBracket,
    TokenInner::Delimeter(Delimeter::ClosedBracket) => ClosedBracket,
    "]",
);

#[derive(Debug)]
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

#[derive(Debug)]
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
            Some(ref imm) => imm
                .value
                .try_into()
                .map_err(|_| spanned_error!(imm.span.clone(), "segment origin out of range"))?,
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
            let origin =
                match dseg.org {
                    Some(ref imm) => match imm.value.try_into().map_err(|_| {
                        spanned_error!(imm.span.clone(), "segment origin out of range")
                    }) {
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

#[derive(Debug)]
pub struct CSeg {
    cseg: Option<token::Cseg>,
    org: Option<Immediate>,
    tokens: Vec<ExpTok>,
}

#[derive(Debug)]
pub struct Context {
    pub code: Vec<CSeg>,
    pub data: Vec<DSeg>,
    current_segment: Segment,
    pub defines: HashMap<String, TokenStream>,
    pub macros: HashMap<String, Macro>,
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

    fn skip_newline(&mut self) {
        if matches!(
            self.peek(),
            Some(Token {
                inner: TokenInner::NewLine,
                span: _
            })
        ) {
            self.position += 1;
        }
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

#[derive(Debug)]
pub enum ExpTok {
    Label(Label),
    Instruction(Inst),
}

impl Parsable for ExpTok {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let name: Ident = ctx.parse()?;

        if let Some(Token {
            span: _,
            inner: TokenInner::Punctuation(Punctuation::Colon),
        }) = ctx.peek()
        {
            let colon: Token![:] = ctx.parse()?;
            let nl: NewLine = ctx.parse()?;

            Ok(ExpTok::Label(Label { name, colon, nl }))
        } else {
            let args = punctuated!(ctx)?;
            // we know there's a newline at the end, so we can just skip it
            ctx.position += 1;

            Ok(ExpTok::Instruction(Inst { name, args }))
        }
    }
}

#[derive(Debug)]
pub struct Inst {
    pub name: Ident,
    pub args: Punctuated<Argument, Token![,]>,
}

#[derive(Debug)]
pub struct Label {
    pub name: Ident,
    pub colon: Token![:],
    pub nl: NewLine,
}

#[derive(Debug)]
pub struct ParseStream {
    pub code: Vec<CSeg>,
    pub data: Vec<DSeg>,
    pub macros: HashMap<String, Macro>,
}

pub fn parse(mut stream: TokenStream) -> Result<ParseStream, Errors> {
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

    while let Some(tok) = ctx.peek().cloned() {
        if let Err(mut err) = expand_preproc(tok, &mut ctx) {
            errors.append(&mut err);
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    } else {
        Ok(ParseStream {
            code: ctx.code,
            data: ctx.data,
            macros: ctx.macros,
        })
    }
}

#[derive(Debug)]
pub enum Argument {
    Reg(Register),
    Immediate(Immediate),
    Addr(Bracketed<TokenStream>),
    Expr(Parenthesized<TokenStream>),
}

impl Parsable for Argument {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        match ctx.peek() {
            Some(Token { span: _, inner: TokenInner::Ident(lex::Ident::Register(_)) }) => Ok(Argument::Reg(ctx.parse()?)),
            Some(Token { span: _, inner: TokenInner::Immediate(_) }) => Ok(Argument::Immediate(ctx.parse()?)),
            Some(Token { span: _, inner: TokenInner::Delimeter(Delimeter::OpenBracket) }) => Ok(Argument::Addr(bracketed!(ctx)?)),
            Some(Token { span: _, inner: TokenInner::Delimeter(Delimeter::OpenParen) }) => Ok(Argument::Expr(parenthesized!(ctx)?)),
            Some(_) => {
                // SAFETY: Since `peek()` returned `Some`, we know `next()` will as well
                let next = unsafe { ctx.next().unwrap_unchecked() };
                Err(spanned_error!(next.span, "expected argument, found {}", next.inner.description()))
            },
            None => Err(error!("expected argument, found `eof`")),
        }
    }
}

fn expand_preproc(peek: Token, ctx: &mut Context) -> Result<(), Errors> {
    use TokenInner as TI;
    match peek.inner {
        TI::Ident(lex::Ident::PreProc(PreProc::Define)) => {
            let def: Define = ctx.parse().map_err(|err| Into::<Errors>::into(err))?;
            ctx.defines.insert(def.name, def.value);
        }
        TI::Ident(lex::Ident::PreProc(PreProc::If)) => {
            eval_if(ctx).map_err(|err| Into::<Errors>::into(err))?
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Org)) => {
            let origin: Org = ctx.parse().map_err(|err| Into::<Errors>::into(err))?;
            if let Some(org) = ctx.current_segment.org() {
                return Err(Diagnostic::referencing_error(
                    origin.span,
                    "duplicate definitions of origin",
                    Reference::new(org.span.clone(), "origin originally defined here"),
                )
                .with_help("`@org` can only be used once per section")
                .into());
            } else {
                *ctx.current_segment.org() = Some(origin.address);
            }
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Cseg)) => {
            let mut segment = Segment::CSeg(CSeg {
                cseg: Some(ctx.parse().map_err(|err| Into::<Errors>::into(err))?),
                org: None,
                tokens: Vec::new(),
            });

            std::mem::swap(&mut segment, &mut ctx.current_segment);

            match segment {
                Segment::CSeg(cseg) => ctx.code.push(cseg),
                Segment::DSeg(dseg) => ctx.data.push(dseg),
            }
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Dseg)) => {
            let mut segment = Segment::DSeg(DSeg {
                dseg: ctx.parse().map_err(|err| Into::<Errors>::into(err))?,
                org: None,
                variables: HashMap::new(),
            });

            std::mem::swap(&mut segment, &mut ctx.current_segment);

            match segment {
                Segment::CSeg(cseg) => ctx.code.push(cseg),
                Segment::DSeg(dseg) => ctx.data.push(dseg),
            }
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Macro)) => {
            let mac: Macro = ctx.parse().map_err(|err| Into::<Errors>::into(err))?;
            let name = mac.name.clone();
            if let Some(first) = ctx.macros.insert(mac.name.value.to_owned(), mac) {
                return Err(Diagnostic::referencing_error(
                    name.span,
                    format!("duplicate definitions of macro `{}`", name.value),
                    Reference::new(first.name.span, "macro originally defined here"),
                )
                .into());
            }
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Include)) => {
            let start = ctx.position;
            ctx.position += 1;
            let path = ctx.parse().map_err(|err| Into::<Errors>::into(err))?;
            let _: NewLine = ctx.parse().map_err(|err| Into::<Errors>::into(err))?;

            let tokens: TokenStream =
                include::include(path).map_err(|err| Into::<Errors>::into(err))?;
            ctx.stream.splice(start..ctx.position, tokens);

            ctx.position = start;
        }
        _ => ctx.position += 1,
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
                ctx.position += 1;
                depth += 1;
            }
            TI::Ident(lex::Ident::PreProc(PreProc::EndIf)) => {
                ctx.position += 1;
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
                            if matches!(more.inner, TI::Ident(lex::Ident::PreProc(PreProc::EndIf)))
                            {
                            } else {
                            }
                        }
                        return Err(error!(""));
                    }
                }
            }
            _ => {
                if eval {
                    // we know we will recieve `Some()` from `next()`,
                    // since we recieved `Some()` from `peek()`.
                    out.push(unsafe { ctx.next().unwrap_unchecked() });
                } else {
                    ctx.position += 1;
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

    if end == 0 {
        return Err(spanned_error!(
            ctx.stream[start].span.clone(),
            "expected expression, found newline"
        ));
    }

    let eval = eval::eval_no_paren(&ctx.stream[start..(start + end)], &ctx.defines)?;

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
            _ => {}
        }
    }

    Err(spanned_error!(
        err_span,
        "unclosed if expression; expected `@endif`, found `eof`"
    ))
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
        let org: Token![@org] = ctx.parse()?;
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
        let _def: Token![@define] = ctx.parse()?;
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
                }
                .into(),
                "define name cannot contain whitespace",
            ));
        }

        let lexed = lex::lex_string(&trimmed[(pos + 1)..]);
        let mut value = match lexed {
            Ok(l) => l,
            Err(errors) => {
                for err in errors {
                    err.emit();
                }
                error!("Unable to lex define due to previous errors").scream();
            }
        };

        Ok(Define { name, value })
    }
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

impl From<Vec<Ty>> for Types {
    fn from(value: Vec<Ty>) -> Self {
        let mut types = Types::empty();

        for ty in value {
            types.insert(match ty.ty {
                lex::Ty::Reg => Types::REG,
                lex::Ty::Addr => Types::ADDR,
                lex::Ty::Label => Types::LABEL,
                lex::Ty::Str => Types::STR,
                lex::Ty::Imm8 => Types::IMM8,
                lex::Ty::Imm16 => Types::IMM16,
                lex::Ty::Imm32 => Types::IMM32,
                lex::Ty::Imm64 => Types::IMM64,
                lex::Ty::Any => Types::all(),
            })
        }

        types
    }
}

#[derive(Debug)]
pub struct Parameter {
    name: MacroVariable,
    types: Types,
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
            TI::Ident(lex::Ident::Register(_)) => self.types.contains(Types::REG),
            TI::Ident(_) => self.types.contains(Types::LABEL),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct MacroDef {
    parameters: Vec<Parameter>,
    expansion: Braced<TokenStream>,
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
        let mut expanded = TokenStream::with_capacity(self.expansion.inner.len());

        let parameters: HashMap<String, &Token> = HashMap::from_iter(
            self.parameters
                .iter()
                .map(|p| p.name.name.to_owned())
                .zip(parameters.into_iter()),
        );

        for token in self.expansion.inner.iter() {
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

    fn parse_inputs(ctx: &mut Context) -> Result<Vec<Parameter>, Diagnostic> {
        let paren: OpenParen = ctx.parse()?;

        let mut params: Vec<Parameter> = Vec::new();
        if matches!(
            ctx.peek(),
            Some(Token {
                span: _,
                inner: TokenInner::Delimeter(Delimeter::ClosedParen)
            })
        ) {
            ctx.position += 1;
            return Ok(params);
        }

        while ctx.peek().is_some() {
            let var: MacroVariable = ctx.parse()?;
            for param in params.iter() {
                if param.name.name == var.name {
                    return Err(spanned_error!(
                        var.span,
                        "duplicate parameter `{}`",
                        var.name
                    ));
                }
            }
            let _seperator: Token![:] = ctx.parse()?;
            let mut types = vec![ctx.parse()?];

            while let Some(tok) = ctx.peek() {
                match tok.inner {
                    TokenInner::Delimeter(Delimeter::ClosedParen) => {
                        let _close: ClosedParen = ctx.parse()?;
                        return Ok(params);
                    }
                    TokenInner::Punctuation(Punctuation::Comma) => {
                        ctx.position += 1;
                        break;
                    }
                    _ => {
                        let _or: Token![|] = ctx.parse()?;
                        let ty: Ty = ctx.parse()?;
                        types.push(ty);
                    }
                }
            }

            params.push(Parameter {
                name: var,
                types: types.into(),
            })
        }

        Err(spanned_error!(
            paren.span,
            "unmatched delimeter; expected closing `)`"
        ))
    }
}

impl Parsable for MacroDef {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let inputs = Self::parse_inputs(ctx)?;
        let expansion: Braced<TokenStream> = braced!(ctx)?;

        Ok(MacroDef {
            parameters: inputs,
            expansion,
        })
    }
}

#[derive(Debug)]
pub struct Macro {
    name: Ident,
    rules: Vec<MacroDef>,
}

impl Macro {
    fn expand(self, span: Arc<Span>, parameters: Vec<Token>) -> Result<TokenStream, Diagnostic> {
        let rule = self
            .rules
            .iter()
            .find(|def| def.fits(&parameters))
            .ok_or_else(|| spanned_error!(span, "no rules matched these arguments"))?;
        rule.expand(&parameters)
    }
}

impl Parsable for Macro {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let proc: Token![@macro] = ctx.parse()?;
        let name: Ident = ctx.parse()?;

        let mut rules = Vec::new();

        match ctx.peek() {
            Some(Token {
                inner: TokenInner::Delimeter(Delimeter::OpenParen),
                span,
            }) => {
                rules.push(ctx.parse()?);
                Ok(Macro { name, rules })
            }
            Some(Token {
                inner: TokenInner::Delimeter(Delimeter::OpenBrace),
                span,
            }) => {
                let brace: OpenBrace = ctx.parse()?;
                ctx.skip_newline();

                while let Some(tok) = ctx.peek() {
                    use TokenInner as TI;
                    match tok.inner {
                        TI::Delimeter(Delimeter::OpenParen) => rules.push(ctx.parse()?),
                        TI::Delimeter(Delimeter::ClosedBrace) => {
                            let _close: ClosedBrace = ctx.parse()?;
                            let _nl: NewLine = ctx.parse()?;
                            return Ok(Macro { name, rules });
                        }
                        TI::NewLine => ctx.position += 1,
                        _ => {
                            return Err(spanned_error!(
                                tok.span.clone(),
                                "expected start of rule definition or end of macro, found {}",
                                tok.inner.description()
                            ))
                        }
                    }
                }

                Err(spanned_error!(
                    brace.span,
                    "unmatched delimeter; expected closing `}}`"
                ))
            }
            Some(tok) => Err(Diagnostic::referencing_error(
                tok.span.clone(),
                format!(
                    "expected argument definition or rule definition, found {}",
                    tok.inner.description()
                ),
                Reference::new(proc.span, "expected as part of this macro"),
            )),
            None => Err(error!(
                "expected argument definition or rule defninition, found `eof`"
            )),
        }
    }
}

pub struct Path {
    open: Token![<],
    pub path: PathInner,
    close: Token![>],
    nl: NewLine,
    pub span: Arc<Span>,
}

impl Path {
    fn span(open: &Token![<], close: &Token![>]) -> Arc<Span> {
        Arc::new(Span {
            line: open.span.line,
            range: open.span.range.start..close.span.range.end,
            source: open.span.source.clone(),
        })
    }
}

impl Parsable for Path {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let open = ctx.parse()?;
        let path = ctx.parse()?;
        let close = ctx.parse()?;
        let span = Path::span(&open, &close);
        Ok(Path {
            open,
            path,
            close,
            nl: ctx.parse()?,
            span,
        })
    }
}

pub enum PathInner {
    Quoted(LitString),
    Unquoted(Punctuated<Ident, Token![/]>),
}

impl Parsable for PathInner {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        if let Some(tok) = ctx.peek() {
            match tok.inner {
                TokenInner::String(_) => return ctx.parse().map(|lit| PathInner::Quoted(lit)),
                _ => {
                    return punctuated!(ctx, TokenInner::Punctuation(Punctuation::Gt))
                        .map(|p| PathInner::Unquoted(p))
                }
            }
        } else {
            Err(error!("expected path, found `eof`"))
        }
    }
}

#[derive(Debug)]
pub enum Address {
    Immediate(Immediate),
    Variable(Variable),
    Label(Ident),
}

impl Parsable for Address {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        match ctx.peek() {
            Some(Token { span: _, inner: TokenInner::Immediate(_) }) => Ok(Address::Immediate(ctx.parse()?)),
            Some(Token { span: _, inner: TokenInner::Ident(lex::Ident::Variable(_)) }) => Ok(Address::Variable(ctx.parse()?)),
            Some(Token { span: _, inner: TokenInner::Ident(lex::Ident::Ident(_)) }) => Ok(Address::Label(ctx.parse()?)),
            Some(_) => {
                let next = unsafe { ctx.next().unwrap_unchecked() };
                Err(spanned_error!(next.span, "expected address, found {}", next.inner.description()))
            },
            None => Err(error!("expected address, found `eof`")),
        }
    }
}

#[derive(Debug)]
pub enum MemAddr {
    Immediate(Immediate),
    Variable(Variable),
}

#[derive(Debug)]
pub enum ProgAddr {
    Immediate(Immediate),
    Label(Ident),
}
