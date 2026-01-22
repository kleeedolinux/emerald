use codespan::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // literals
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    CharLiteral(char),
    StringLiteral(String),
    Null,

    // idntifiers
    Identifier(String),

    // keywords
    Def,
    Return,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Struct,
    Trait,
    Implement,
    Module,
    Require,
    Use,
    Foreign,
    Comptime,
    Declare,
    End,
    Uses,
    Returns,
    Do,
    Mut,
    At,
    Ref,
    RefNullable,

    // typs
    Void,
    Byte,
    Int,
    Long,
    Size,
    Float,
    Bool,
    Char,
    String,

    // operators
    Plus,           // +
    Minus,          // 
    Star,           // *
    Slash,          // /
    Percent,        // %
    Equal,          // =
    EqualEqual,     // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    And,            // &&
    Or,             // ||
    Not,            // !
    Dot,            // 
    Comma,          // 
    Colon,          // :
    ColonColon,     // ::
    Semicolon,      // ;
    Question,       // ?
    Exists,         // exisst?
    Ellipsis,       // ...

    // dlmtrs
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    Pipe,           // |

    // spcl
    Eof,
    Error(String),
}

impl TokenKind {
    pub fn is_keyword(s: &str) -> bool {
        matches!(
            s,
            "def" | "return" | "if" | "else" | "while" | "for" | "break" | "continue"
                | "struct" | "trait" | "implement" | "module" | "require" | "use"
                | "foreign" | "comptime" | "declare" | "end" | "uses" | "returns"
                | "do" | "mut" | "at" | "ref" | "null" | "not" | "void" | "byte" | "int"
                | "long" | "size" | "float" | "bool" | "char" | "string"
        )
    }

    pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
        match s {
            "def" => Some(TokenKind::Def),
            "return" => Some(TokenKind::Return),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            "struct" => Some(TokenKind::Struct),
            "trait" => Some(TokenKind::Trait),
            "implement" => Some(TokenKind::Implement),
            "module" => Some(TokenKind::Module),
            "require" => Some(TokenKind::Require),
            "use" => Some(TokenKind::Use),
            "foreign" => Some(TokenKind::Foreign),
            "comptime" => Some(TokenKind::Comptime),
            "declare" => Some(TokenKind::Declare),
            "end" => Some(TokenKind::End),
            "uses" => Some(TokenKind::Uses),
            "returns" => Some(TokenKind::Returns),
            "do" => Some(TokenKind::Do),
            "mut" => Some(TokenKind::Mut),
            "at" => Some(TokenKind::At),
            "ref" => Some(TokenKind::Ref),
            "null" => Some(TokenKind::Null),
            "not" => Some(TokenKind::Not),
            "void" => Some(TokenKind::Void),
            "byte" => Some(TokenKind::Byte),
            "int" => Some(TokenKind::Int),
            "long" => Some(TokenKind::Long),
            "size" => Some(TokenKind::Size),
            "float" => Some(TokenKind::Float),
            "bool" => Some(TokenKind::Bool),
            "char" => Some(TokenKind::Char),
            "string" => Some(TokenKind::String),
            "true" => Some(TokenKind::BoolLiteral(true)),
            "false" => Some(TokenKind::BoolLiteral(false)),
            _ => None,
        }
    }
}
