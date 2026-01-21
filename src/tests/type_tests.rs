use crate::core::types::ty::Type;
use crate::core::types::primitive::PrimitiveType;
use crate::core::types::size_calculator::SizeCalculator;
use crate::core::types::composite::StructType;

#[test]
fn test_primitive_type_sizes() {
    assert_eq!(PrimitiveType::Byte.size_in_bytes(), 1);
    assert_eq!(PrimitiveType::Int.size_in_bytes(), 4);
    assert_eq!(PrimitiveType::Long.size_in_bytes(), 8);
    assert_eq!(PrimitiveType::Float.size_in_bytes(), 8);
    assert_eq!(PrimitiveType::Bool.size_in_bytes(), 1);
    assert_eq!(PrimitiveType::Char.size_in_bytes(), 4);
}

#[test]
fn test_type_size() {
    let int_type = Type::Primitive(PrimitiveType::Int);
    assert_eq!(int_type.size_in_bytes(), Some(4));
    
    let string_type = Type::String;
    assert_eq!(string_type.size_in_bytes(), Some(16)); // 2 * usize
    
    let ptr_type = Type::Pointer(crate::core::types::pointer::PointerType::new(
        Type::Primitive(PrimitiveType::Int),
        false,
    ));
    assert_eq!(ptr_type.size_in_bytes(), Some(8)); // ptr size
}

#[test]
fn test_struct_size_calculation() {
    let mut calculator = SizeCalculator::new();
    let struct_type = StructType {
        name: "Point".to_string(),
        fields: vec![
            crate::core::types::composite::Field {
                name: "x".to_string(),
                type_: Type::Primitive(PrimitiveType::Int),
                offset: None,
            },
            crate::core::types::composite::Field {
                name: "y".to_string(),
                type_: Type::Primitive(PrimitiveType::Int),
                offset: None,
            },
        ],
        size: None,
        align: None,
    };
    
    let size = calculator.calculate_size(&struct_type).unwrap();
    assert_eq!(size, 8); // 2 * int
}

#[test]
fn test_cycle_detection() {
    let mut calculator = SizeCalculator::new();
    
    // crt two strcts that ref each other by vl
    let struct_a = StructType {
        name: "A".to_string(),
        fields: vec![
            crate::core::types::composite::Field {
                name: "b".to_string(),
                type_: Type::Struct(StructType {
                    name: "B".to_string(),
                    fields: vec![
                        crate::core::types::composite::Field {
                            name: "a".to_string(),
                            type_: Type::Struct(StructType {
                                name: "A".to_string(),
                                fields: Vec::new(),
                                size: None,
                                align: None,
                            }),
                            offset: None,
                        },
                    ],
                    size: None,
                    align: None,
                }),
                offset: None,
            },
        ],
        size: None,
        align: None,
    };
    
    // this should detect a cycle
    let result = calculator.calculate_size(&struct_a);
    assert!(result.is_err());
}

#[test]
fn test_circular_dependency_with_forward_decl() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
declare struct Parent
struct Child
  dad : ref Parent
end
struct Parent
  favorite : Child
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // shld compile successfully ptr brks the cycle
    assert!(!reporter.has_errors());
}

#[test]
fn test_circular_dependency_error() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
struct A
  b : B
end
struct B
  a : A
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // this shld produce an err about crclr dependency
    // both a and b contain each othr by vl
    assert!(reporter.has_errors());
}

#[test]
fn test_circular_dependency_multiple_structs() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
struct A
  b : B
end
struct B
  c : C
end
struct C
  a : A
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // shld prdc an err a > b > c > a cycle
    assert!(reporter.has_errors());
}

#[test]
fn test_circular_dependency_broken_by_multiple_pointers() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
declare struct Node
struct Node
  value : int
  next : ref? Node
  prev : ref? Node
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // should cmpl successfully pointers brk cycls
    assert!(!reporter.has_errors());
}

#[test]
fn test_shadowing_basic() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
def test
  x : int = 10
  if true
    x : string = "shadowed"
  end
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // shld compile sccssflly shadowing is allowed
    assert!(!reporter.has_errors());
}

#[test]
fn test_shadowing_nested() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
def test
  x : int = 1
  if true
    x : int = 2
    if true
      x : int = 3
    end
  end
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // shld compile successfully nested shadowing is allowed
    assert!(!reporter.has_errors());
}

#[test]
fn test_comptime_nested() {
    use crate::error::Reporter;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic::SemanticAnalyzer;
    use codespan::Files;

    let source = r#"
def test returns int
  x = comptime 5 + 3
  return x
end
"#;
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    // should cmpl sccssflly nstd comptime exprssns
    assert!(!reporter.has_errors());
}
