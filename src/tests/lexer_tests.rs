use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::lexer::token::TokenKind;
use codespan::Files;

#[test]
fn test_lexer_literals() {
    let mut files = Files::new();
    let file_id = files.add("test.em", "42 3.14 true false 'c' \"hello\" null".to_string());
    let mut reporter = Reporter::new();
    let source = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source, file_id, &mut reporter);
    let tokens = lexer.tokenize();

    assert_eq!(tokens.len(), 8); // 7 tokens + eof
    assert!(matches!(tokens[0].kind, TokenKind::IntLiteral(42)));
    assert!(matches!(tokens[1].kind, TokenKind::FloatLiteral(_)));
    assert!(matches!(tokens[2].kind, TokenKind::BoolLiteral(true)));
    assert!(matches!(tokens[3].kind, TokenKind::BoolLiteral(false)));
    assert!(matches!(tokens[4].kind, TokenKind::CharLiteral(_)));
    assert!(matches!(tokens[5].kind, TokenKind::StringLiteral(_)));
    assert!(matches!(tokens[6].kind, TokenKind::Null));
}

#[test]
fn test_lexer_keywords() {
    let mut files = Files::new();
    let file_id = files.add("test.em", "def struct trait implement module".to_string());
    let mut reporter = Reporter::new();
    let source = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source, file_id, &mut reporter);
    let tokens = lexer.tokenize();

    assert!(matches!(tokens[0].kind, TokenKind::Def));
    assert!(matches!(tokens[1].kind, TokenKind::Struct));
    assert!(matches!(tokens[2].kind, TokenKind::Trait));
    assert!(matches!(tokens[3].kind, TokenKind::Implement));
    assert!(matches!(tokens[4].kind, TokenKind::Module));
}

#[test]
fn test_lexer_operators() {
    let mut files = Files::new();
    let file_id = files.add("test.em", "+ - * / % == != < <= > >=".to_string());
    let mut reporter = Reporter::new();
    let source = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source, file_id, &mut reporter);
    let tokens = lexer.tokenize();

    assert!(matches!(tokens[0].kind, TokenKind::Plus));
    assert!(matches!(tokens[1].kind, TokenKind::Minus));
    assert!(matches!(tokens[2].kind, TokenKind::Star));
    assert!(matches!(tokens[3].kind, TokenKind::Slash));
    assert!(matches!(tokens[4].kind, TokenKind::Percent));
    assert!(matches!(tokens[5].kind, TokenKind::EqualEqual));
    assert!(matches!(tokens[6].kind, TokenKind::NotEqual));
    assert!(matches!(tokens[7].kind, TokenKind::Less));
    assert!(matches!(tokens[8].kind, TokenKind::LessEqual));
    assert!(matches!(tokens[9].kind, TokenKind::Greater));
    assert!(matches!(tokens[10].kind, TokenKind::GreaterEqual));
}

#[test]
fn test_lexer_string_escape() {
    let mut files = Files::new();
    let file_id = files.add("test.em", "\"hello\\nworld\"".to_string());
    let mut reporter = Reporter::new();
    let source = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source, file_id, &mut reporter);
    let tokens = lexer.tokenize();

    if let TokenKind::StringLiteral(s) = &tokens[0].kind {
        assert!(s.contains('\n'));
    } else {
        panic!("Expected string literal");
    }
}
