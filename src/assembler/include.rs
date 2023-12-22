use super::{
    lex::{self, TokenStream},
    parse::{Path, PathInner},
    Errors,
};
use crate::spanned_error;

use clio::Input;
use phf::{phf_map, Map};

static BUILT_INS: Map<&'static str, &str> = phf_map! {
    "arithmetic" => include_str!("../../std/arithmetic.asm"),
    "flow" => include_str!("../../std/flow.asm"),
    "util" => include_str!("../../std/util.asm"),
};

pub fn include(path: Path) -> Result<TokenStream, Errors> {
    match path.path {
        PathInner::Quoted(s) => lex::lex(
            Input::new(&s.value.to_string())
                .map_err(|err| vec![spanned_error!(s.span, "unable to read input; {err}")])?,
        ),
        PathInner::Unquoted(p) => {
            let locator = p
                .values()
                .map(|val| val.value.as_str())
                .collect::<Vec<&str>>()
                .join("/");

            BUILT_INS
                .get_entry(&locator)
                .ok_or_else(|| {
                    vec![spanned_error!(
                        path.span,
                        "built-in module `{locator}` not recognized"
                    )]
                })
                .and_then(|builtin| lex::lex_string(Some(*builtin.0), *builtin.1))
        }
    }
}
