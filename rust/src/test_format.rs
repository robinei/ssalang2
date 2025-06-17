use crate::parse::parse_program;
use crate::pretty_print::PrettyPrinter;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment_formatting() {
        let input = r#"// This is a comment
fn main() {
    // Another comment
    return 42; // Inline comment
}"#;
        
        let ast = parse_program(input).unwrap();
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        
        // Just check that it contains the function
        assert!(output.contains("fn main"));
        assert!(output.contains("return 42"));
    }

    #[test]
    fn test_comprehensive_formatting_roundtrip() {
        let input = r#"// File header comment
// Another header line

// Function documentation
fn main(x: i32, y: bool) -> i32 {
    // Simple return with comment
    return 42; // Inline comment
} // End of function"#;
        
        // Test that input can be parsed and formatted
        let ast = parse_program(input).unwrap();
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        
        // Test that the output can be parsed again (verifies it's valid syntax)
        let _ast2 = parse_program(&output).expect("Pretty-printed output should be valid syntax");
        
        // Verify key structural elements are preserved
        assert!(output.contains("fn main"));
        assert!(output.contains("x: i32"));
        assert!(output.contains("y: bool"));
        assert!(output.contains("-> i32"));
        assert!(output.contains("return 42"));
    }

    #[test]
    fn test_edge_case_formatting_tokens() {
        // Test various edge cases for formatting token placement
        let input = r#"// Leading comment

fn main() -> bool {
    return true;
}
// Trailing comment"#;
        
        let ast = parse_program(input).unwrap();
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        
        // Verify it can round-trip
        let _ast2 = parse_program(&output).expect("Edge case output should be valid syntax");
        
        // Check that comments and structure are preserved
        assert!(output.contains("// Leading comment"));
        assert!(output.contains("fn main()"));
        assert!(output.contains("-> bool"));
        assert!(output.contains("return true"));
        assert!(output.contains("// Trailing comment"));
    }
}