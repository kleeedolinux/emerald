use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::core::types::ty::Type;
use codespan::Files;

fn analyze_source(source: &str) -> (crate::core::ast::Ast, Reporter) {
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
    
    if reporter.has_errors() {
        for diag in reporter.diagnostics() {
            eprintln!("[{:?}] {:?}: {}", diag.kind, diag.severity, diag.message);
        }
    }
    
    (ast, reporter)
}

#[test]
fn test_trait_object_type_size() {
    let trait_obj = Type::TraitObject(crate::core::types::ty::TraitObjectType {
        trait_name: "Drawable".to_string(),
        constraints: Vec::new(),
    });
    
    // trait objects shld be 2 pointers (data + vtable)
    assert_eq!(trait_obj.size_in_bytes(), Some(16)); // 2 * usize (8 bytes each)
}

#[test]
fn test_trait_object_type_align() {
    let trait_obj = Type::TraitObject(crate::core::types::ty::TraitObjectType {
        trait_name: "Drawable".to_string(),
        constraints: Vec::new(),
    });
    
    // trait objects shld align 2 pointer size
    assert_eq!(trait_obj.align(), 8);
}

#[test]
fn test_trait_object_basic() {
    let source = r#"
trait Drawable
  def draw(self)
end

struct Circle
  radius : float
end

implement Drawable for Circle
  def draw(self : Circle)
    // draw circle
  end
end

def main
  // trait objects wld be used here when implemented
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_trait_object_with_constraints() {
    let trait_obj = Type::TraitObject(crate::core::types::ty::TraitObjectType {
        trait_name: "Clone".to_string(),
        constraints: vec!["Send".to_string(), "Sync".to_string()],
    });
    
    assert_eq!(trait_obj.size_in_bytes(), Some(16));
}

#[test]
fn test_trait_object_in_dependency_graph() {
    use crate::core::types::dependency::DependencyGraph;
    
    let trait_obj = Type::TraitObject(crate::core::types::ty::TraitObjectType {
        trait_name: "Display".to_string(),
        constraints: Vec::new(),
    });
    
    // trait objects shldnt create struct dependencies
    let deps = DependencyGraph::extract_dependencies(&trait_obj);
    assert!(deps.is_empty());
}

#[test]
fn test_trait_object_type_representation() {
    // test that trait object type is properly represented
    let trait_obj = Type::TraitObject(crate::core::types::ty::TraitObjectType {
        trait_name: "Display".to_string(),
        constraints: vec!["Clone".to_string()],
    });
    
    // verify type properties
    assert_eq!(trait_obj.size_in_bytes(), Some(16));
    assert_eq!(trait_obj.align(), 8);
    
    // trait objects shldnt be primitive or struct
    assert!(!trait_obj.is_struct());
    assert!(!trait_obj.is_array());
    assert!(!trait_obj.is_pointer());
}
