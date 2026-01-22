use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::lexer::token::{Token, TokenKind};
use codespan::{ByteIndex, FileId, Span};

pub struct Lexer<'a> {
    source: &'a str,
    file_id: FileId,
    reporter: &'a mut Reporter,
    current: usize,
    start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file_id: FileId, reporter: &'a mut Reporter) -> Self {
        Self {
            source,
            file_id,
            reporter,
            current: 0,
            start: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            self.start = self.current;
            let token = self.next_token();
            tokens.push(token.clone());

            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
        }

        tokens
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        self.start = self.current;
        let c = self.advance();

        match c {
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '[' => self.make_token(TokenKind::LeftBracket),
            ']' => self.make_token(TokenKind::RightBracket),
            ',' => self.make_token(TokenKind::Comma),
            ';' => self.make_token(TokenKind::Semicolon),
            ':' => {
                if self.match_char(':') {
                    self.make_token(TokenKind::ColonColon)
                } else {
                    self.make_token(TokenKind::Colon)
                }
            },
            '|' => {
                if self.match_char('|') {
                    self.make_token(TokenKind::Or)
                } else {
                    self.make_token(TokenKind::Pipe)
                }
            },
            '+' => self.make_token(TokenKind::Plus),
            '-' => self.make_token(TokenKind::Minus),
            '*' => self.make_token(TokenKind::Star),
            '/' => {
                if self.match_char('/') {
                    // line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    self.next_token()
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '#' => {
                // line comment
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
                self.next_token()
            }
            '%' => self.make_token(TokenKind::Percent),
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::NotEqual)
                } else {
                    self.make_token(TokenKind::Not)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.make_token(TokenKind::And)
                } else {
                    self.error_token("Unexpected character '&'")
                }
            }
            '@' => self.make_token(TokenKind::At),
            '.' => {
                if self.peek() == '.' && self.peek_next() == '.' {
                    self.advance(); // consume second .
                    self.advance(); // consume third .
                    self.make_token(TokenKind::Ellipsis)
                } else {
                    self.make_token(TokenKind::Dot)
                }
            },
            '?' => {
                if self.peek() == '?' {
                    // chk 4 exists?
                    let saved = self.current;
                    self.advance(); // cnsm first ?
                    if self.peek() == '?' {
                        self.advance(); // consume second ?
                        if self.is_alpha(self.peek()) {
                            let start = self.current;
                            while self.is_alphanumeric(self.peek()) {
                                self.advance();
                            }
                            let text = &self.source[start..self.current];
                            if text == "exists" {
                                return self.make_token(TokenKind::Exists);
                            }
                        }
                        self.current = saved;
                    } else {
                        self.current = saved;
                    }
                }
                self.make_token(TokenKind::Question)
            }
            '"' => self.string(),
            '\'' => self.char_literal(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token(&format!("Unexpected character '{}'", c)),
        }
    }

    fn string(&mut self) -> Token {
        let mut value = String::new();
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                return self.error_token("Unterminated string literal");
            }
            if self.peek() == '\\' {
                self.advance();
                match self.peek() {
                    'n' => {
                        value.push('\n');
                        self.advance();
                    }
                    't' => {
                        value.push('\t');
                        self.advance();
                    }
                    'r' => {
                        value.push('\r');
                        self.advance();
                    }
                    '\\' => {
                        value.push('\\');
                        self.advance();
                    }
                    '"' => {
                        value.push('"');
                        self.advance();
                    }
                    _ => {
                        value.push('\\');
                    }
                }
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string literal");
        }

        self.advance(); // cnsm closing "
        self.make_token(TokenKind::StringLiteral(value))
    }

    fn char_literal(&mut self) -> Token {
        if self.is_at_end() {
            return self.error_token("Unterminated character literal");
        }

        let c = if self.peek() == '\\' {
            self.advance();
            match self.peek() {
                'n' => {
                    self.advance();
                    '\n'
                }
                't' => {
                    self.advance();
                    '\t'
                }
                'r' => {
                    self.advance();
                    '\r'
                }
                '\\' => {
                    self.advance();
                    '\\'
                }
                '\'' => {
                    self.advance();
                    '\''
                }
                _ => self.advance(),
            }
        } else {
            self.advance()
        };

        if self.peek() != '\'' {
            return self.error_token("Unterminated character literal");
        }

        self.advance(); // conume closing
        self.make_token(TokenKind::CharLiteral(c))
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // look 4 fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // cnsm
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            let text = &self.source[self.start..self.current];
            let value: f64 = text.parse().unwrap_or(0.0);
            self.make_token(TokenKind::FloatLiteral(value))
        } else {
            let text = &self.source[self.start..self.current];
            let value: i64 = text.parse().unwrap_or(0);
            self.make_token(TokenKind::IntLiteral(value))
        }
    }

    fn identifier(&mut self) -> Token {
        while self.is_alphanumeric(self.peek()) || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        // chk 4 ref?
        if text == "ref" && self.peek() == '?' {
            self.advance(); // consume ?
            return self.make_token(TokenKind::RefNullable);
        }

        // chk 4 bln literals first
        match text {
            "true" => return self.make_token(TokenKind::BoolLiteral(true)),
            "false" => return self.make_token(TokenKind::BoolLiteral(false)),
            _ => {}
        }
        
        match TokenKind::keyword_from_str(text) {
            Some(kind) => self.make_token(kind),
            None => self.make_token(TokenKind::Identifier(text.to_string())),
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap_or('\0')
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let span = Span::new(
            ByteIndex(self.start as u32),
            ByteIndex(self.current as u32),
        );
        Token { kind, span }
    }

    fn error_token(&mut self, message: &str) -> Token {
        let span = Span::new(
            ByteIndex(self.start as u32),
            ByteIndex(self.current as u32),
        );
        let diagnostic = Diagnostic::error(
            DiagnosticKind::LexicalError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
        Token {
            kind: TokenKind::Error(message.to_string()),
            span,
        }
    }
}
