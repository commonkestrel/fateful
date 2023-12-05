use std::sync::Arc;

use crate::{ error, spanned_error };
use super::ascii::AsciiStr;
use super::lex::{self, Span, Delimeter, PreProc, Punctuation, Token, TokenInner, TokenStream};
use super::parse::{Context, Parsable};
use super::Diagnostic;

/// A type-macro that expands to the name of the Rust type representation of a given token.
///
/// Commonly used in struct fields, the type of a `let` statement, or generics for a [`parse()`][Parsable::parse] call.
///
/// Same idea as [syn](https://docs.rs/syn/latest/syn/macro.Token.html).
#[macro_export]
macro_rules! Token {
    [=] => {$crate::assembler::token::Eq};
    [==] => {$crate::assembler::token::EqEq};
    [!=] => {$crate::assembler::token::Ne};
    [<] => {$crate::assembler::token::Lt};
    [<=] => {$crate::assembler::token::Le};
    [>] => {$crate::assembler::token::Gt};
    [>=] => {$crate::assembler::token::Ge};
    [&] => {$crate::assembler::token::And};
    [&&] => {$crate::assembler::token::AndAnd};
    [|] => {$crate::assembler::token::Or};
    [||] => {$crate::assembler::token::OrOr};
    [^] => {$crate::assembler::token::Caret};
    [!] => {$crate::assembler::token::Not};
    [~] => {$crate::assembler::token::Not};
    [+] => {$crate::assembler::token::Plus};
    [-] => {$crate::assembler::token::Minus};
    [/] => {$crate::assembler::token::Slash};
    [*] => {$crate::assembler::token::Star};
    [<<] => {$crate::assembler::token::Shl};
    [>>] => {$crate::assembler::token::Shr};
    [,] => {$crate::assembler::token::Comma};
    [:] => {$crate::assembler::token::Colon};
    [@MACRO] => {$crate::assembler::token::Macro};
    [@DEFINE] => {$crate::assembler::token::Define};
    [@UNDEF] => {$crate::assembler::token::UnDef};
    [@IFDEF] => {$crate::assembler::token::IfDef};
    [@IFNDEF] => {$crate::assembler::token::IfNDef};
    [@IF] => {$crate::assembler::token::If};
    [@ELIF] => {$crate::assembler::token::Elif};
    [@ELSE] => {$crate::assembler::token::Else};
    [@ENDIF] => {$crate::assembler::token::EndIf};
    [@ORG] => {$crate::assembler::token::Org};
    [@CSEG] => {$crate::assembler::token::Cseg};
    [@DSEG] => {$crate::assembler::token::Dseg};
    [@BYTE] => {$crate::assembler::token::Byte};
    [@DOUBLE] => {$crate::assembler::token::Double};
    [@QUAD] => {$crate::assembler::token::Quad};
    [@VAR] => {$crate::assembler::token::Var};
}

/// Creates a struct for a varient of [`TokenInner`][crate::lex::TokenInner] and implements [`Parse`] for it.
///
/// # Examples
///
/// `parsable!{ "+":  }
macro_rules! parsable {
    ($($symbol:literal$(, $alt:literal)?; match $token:ident($inner:pat) => $name:ident$({$($v:vis $field:ident: $ty:ty)*})?),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: ::std::sync::Arc<$crate::assembler::lex::Span>,
                $($($v $field: $ty),*)?
            }

            impl $crate::assembler::parse::Parsable for $name {
                fn parse(ctx: &mut $crate::assembler::parse::Context) -> Result<Self, Diagnostic> {
                    match ctx.next() {
                        Some($crate::assembler::lex::Token { inner: $crate::assembler::lex::TokenInner::$token($inner), span }) => Ok($name{ span: span, $($($field: ::std::borrow::ToOwned::to_owned($field)),*)? }),
                        Some(tok) => Err(spanned_error!(tok.span, concat!("expected `", $symbol, "`"$(, "or `", $alt)?, ", found {}"), tok.inner.description())),
                        _ => Err(error!(concat!("expected `", $symbol, "`"$(, "or `", $alt)?, ", found `eof`"))),
                    }
                }
            }
        )*
    };
    ($($($symbol:ident)+; match $token:ident($inner:pat) => $name:ident$({$($v:vis $field:ident: $ty:ty)*})?),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: ::std::sync::Arc<$crate::assembler::lex::Span>,
                $($($v $field: $ty),*)?
            }

            impl $crate::assembler::parse::Parsable for $name {
                fn parse(ctx: &mut $crate::assembler::parse::Context) -> Result<Self, Diagnostic> {
                    match ctx.next() {
                        Some($crate::assembler::lex::Token { inner: $crate::assembler::lex::TokenInner::$token($inner), span }) => Ok($name{ span: span, $($($field: $field),*)? }),
                        Some(tok) => Err(spanned_error!(tok.span, concat!("expected", $(" ", stringify!($symbol)),+, "found {}"), tok.inner.description())),
                        _ => Err(error!(concat!("expected",$(" ", stringify!($symbol)),+, ", found `eof`"))),
                    }
                }
            }
        )*
    };
}

/* Delimeters */

parsable! {
    '(' ; match Delimeter(Delimeter::OpenParen) => OpenParen,
    ')' ; match Delimeter(Delimeter::ClosedParen) => ClosedParen,
    '[' ; match Delimeter(Delimeter::OpenBracket) => OpenBracket,
    ']' ; match Delimeter(Delimeter::ClosedBracket) => ClosedBracket,
    "{{"; match Delimeter(Delimeter::OpenBrace) => OpenBrace,
    "}}"; match Delimeter(Delimeter::OpenBrace) => ClosedBrace,
}

/* Punctuation */

parsable! {
    '='     ; match Punctuation(Punctuation::Eq) => Eq,
    "=="    ; match Punctuation(Punctuation::EqEq) => EqEq,
    "!="    ; match Punctuation(Punctuation::Ne) => Ne,
    '<'     ; match Punctuation(Punctuation::Lt) => Lt,
    "<="    ; match Punctuation(Punctuation::Le) => Le,
    '>'     ; match Punctuation(Punctuation::Gt) => Gt,
    ">="    ; match Punctuation(Punctuation::Ge) => Ge,
    '&'     ; match Punctuation(Punctuation::And) => And,
    "&&"    ; match Punctuation(Punctuation::AndAnd) => AndAnd,
    '|'     ; match Punctuation(Punctuation::Or) => Or,
    "||"    ; match Punctuation(Punctuation::OrOr) => OrOr,
    '^'     ; match Punctuation(Punctuation::Caret) => Caret,
    '!', '~'; match Punctuation(Punctuation::Not) => Not,
    '+'     ; match Punctuation(Punctuation::Plus) => Plus,
    '-'     ; match Punctuation(Punctuation::Minus) => Minus,
    '/'     ; match Punctuation(Punctuation::Slash) => Slash,
    '*'     ; match Punctuation(Punctuation::Star) => Star,
    "<<"    ; match Punctuation(Punctuation::Shl) => Shl,
    ">>"    ; match Punctuation(Punctuation::Shr) => Shr,
    ','     ; match Punctuation(Punctuation::Comma) => Comma,
    ':'     ; match Punctuation(Punctuation::Colon) => Colon,
}

/* Pre-proc arguments */

parsable! {
    "@include"; match Ident(lex::Ident::PreProc(PreProc::Include)) => Include,
    "@macro"  ; match Ident(lex::Ident::PreProc(PreProc::Macro)) => Macro,
    "@define" ; match Ident(lex::Ident::PreProc(PreProc::Define)) => Define,
    "@undef"  ; match Ident(lex::Ident::PreProc(PreProc::UnDef)) => UnDef,
    "@ifdef"  ; match Ident(lex::Ident::PreProc(PreProc::IfDef)) => IfDef,
    "@ifndef" ; match Ident(lex::Ident::PreProc(PreProc::IfNDef)) => IfNDef,
    "@if"     ; match Ident(lex::Ident::PreProc(PreProc::If)) => If,
    "@else"   ; match Ident(lex::Ident::PreProc(PreProc::Else)) => Else,
    "@elif"   ; match Ident(lex::Ident::PreProc(PreProc::ElIf)) => ElIf,
    "@endif"  ; match Ident(lex::Ident::PreProc(PreProc::EndIf)) => EndIf,
    "@org"    ; match Ident(lex::Ident::PreProc(PreProc::Org)) => Org,
    "@cseg"   ; match Ident(lex::Ident::PreProc(PreProc::Cseg)) => Cseg,
    "@dseg"   ; match Ident(lex::Ident::PreProc(PreProc::Dseg)) => Dseg,
    "@byte"   ; match Ident(lex::Ident::PreProc(PreProc::Byte)) => Byte,
    "@double" ; match Ident(lex::Ident::PreProc(PreProc::Double)) => Double,
    "@quad"   ; match Ident(lex::Ident::PreProc(PreProc::Quad)) => Quad,
    "@var"    ; match Ident(lex::Ident::PreProc(PreProc::Var)) => Var,
}

/* Identifiers */

parsable! {
    register; match Ident(lex::Ident::Register(inner)) => Register{pub inner: lex::Register},
    identifier; match Ident(lex::Ident::Ident(value)) => Ident{pub value: String},
    type; match Ident(lex::Ident::Ty(ty)) => Ty{pub ty: lex::Ty},
}

/* Literals */

parsable! {
    integer literal; match Immediate(value) => Immediate{pub value: i128},
    string literal; match String(value) => LitString{pub value: AsciiStr},
    character literal; match Char(ascii) => Char{pub ascii: u8},
    address; match Address(addr) => Address{pub addr: super::lex::Address},
    doc string; match Doc(md) => Doc{pub md: String},
}

pub struct NewLine {
    span: Arc<Span>,
}

impl Parsable for NewLine {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        match ctx.next() {
            Some(Token { span, inner: TokenInner::NewLine }) => Ok(NewLine { span }),
            Some(tok) => Err(spanned_error!(tok.span, "expected newline, found {}", tok.inner.description())),
            None => Err(error!("expected newline"))
        }
    }
}

impl Parsable for TokenStream {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        let mut stream = Vec::new();
        for tok in ctx {
            if let Token {
                span: _,
                inner: TokenInner::NewLine,
            } = tok
            {
                return Ok(stream);
            }

            stream.push(tok.clone());
        }
        Ok(stream)
    }
}

impl Parsable for Token {
    fn parse(ctx: &mut Context) -> Result<Self, Diagnostic> {
        match ctx.next() {
            Some(tok) => Ok(tok.clone()),
            None => Err(error!("expected token, found `eof`")),
        }
    }
}