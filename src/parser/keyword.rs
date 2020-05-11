use crate::parser::lexer::TokenKind;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Keyword {
	// Flow control
	While,
	Loop,
	For,
	If,
	Else,
	Return,

	// Types
	Union,
	Enum,
	Extern,
        Mutable,
        Null,

	// Namespaces
	Use,
	Let,
	Module,
	Const,
	TypeDef,
	Alias,
	Load,
}

impl PartialEq<TokenKind> for Keyword {
    fn eq(&self, other: &TokenKind) -> bool {
        match other {
            TokenKind::Keyword(keyword) if keyword == self => true,
            _ => false,
        }
    }
}
