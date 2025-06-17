use ssalang2::lexer::{Lexer, TokenType};

fn main() {
    println!("🔍 Token Size Analysis");
    println!("=====================");
    
    // Show token type sizes
    println!("TokenType size: {} bytes", std::mem::size_of::<TokenType>());
    println!("Token size: {} bytes", std::mem::size_of::<ssalang2::lexer::Token>());
    
    // Test with some sample code
    let input = r#"
        fn fibonacci(n: i32) -> i32 {
            if n == 0 {
                return 0;
            } else {
                let result = n + 42;
                return result;
            }
        }
    "#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    
    println!("\n📊 Tokenization Results");
    println!("=======================");
    println!("Input length: {} bytes", input.len());
    println!("Number of tokens: {}", tokens.len());
    println!("Total token storage: {} bytes", 
             tokens.len() * std::mem::size_of::<ssalang2::lexer::Token>());
    
    let total_text_bytes: usize = tokens.iter()
        .map(|token| token.length as usize)
        .sum();
    println!("Total text referenced: {} bytes", total_text_bytes);
    
    println!("\n🔤 Sample Tokens");
    println!("================");
    for (i, token) in tokens.iter().take(10).enumerate() {
        if token.length > 0 {
            let text = lexer.get_token_text(token);
            println!("Token {}: {:?} = \"{}\" (pos: {}, len: {})", 
                     i, token.token_type, text, token.source_loc, token.length);
        } else {
            println!("Token {}: {:?} (pos: {}, len: {})", 
                     i, token.token_type, token.source_loc, token.length);
        }
    }
    
    // Show efficiency
    println!("\n⚡ Efficiency Analysis");
    println!("=====================");
    println!("If we stored strings in tokens directly:");
    let estimated_old_size: usize = tokens.iter()
        .map(|token| {
            match token.token_type {
                TokenType::Identifier | TokenType::IntLiteral | TokenType::StringLiteral => {
                    // Estimated: discriminant + String (24 bytes on 64-bit) + actual text
                    8 + 24 + token.length as usize
                }
                _ => 8, // Just discriminant
            }
        })
        .sum();
    
    let actual_size = tokens.len() * std::mem::size_of::<ssalang2::lexer::Token>();
    
    println!("Estimated old approach: {} bytes", estimated_old_size);
    println!("New approach: {} bytes", actual_size);
    println!("Space saved: {} bytes ({:.1}% reduction)", 
             estimated_old_size.saturating_sub(actual_size),
             if estimated_old_size > 0 { 
                 100.0 * (estimated_old_size.saturating_sub(actual_size) as f64) / (estimated_old_size as f64)
             } else { 0.0 });
}