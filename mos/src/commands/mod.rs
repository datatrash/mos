use ansi_term::ANSIGenericString;
use std::borrow::Cow;

pub use build::*;
pub use format::*;
pub use init::*;
pub use lsp::*;
pub use test::*;

mod build;
mod format;
mod init;
mod lsp;
mod test;

pub fn paint<'a, I, S: 'a + ToOwned + ?Sized>(
    use_color: bool,
    colour: ansi_term::Colour,
    input: I,
) -> ANSIGenericString<'a, S>
where
    I: Into<Cow<'a, S>>,
    <S as ToOwned>::Owned: std::fmt::Debug,
{
    if use_color {
        colour.paint(input)
    } else {
        let input = input.into();
        input.into()
    }
}
