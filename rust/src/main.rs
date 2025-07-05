use ssalang2::astprint::AstPrinter;
use ssalang2::parse::{Parser, ParseError};
use ssalang2::lexer::Lexer;
use std::env;
use std::fs;
use std::process;

/// Print a detailed error message with source context
fn print_error_with_context(error: &ParseError, source: &str, filename: &str, context_lines: usize) {
    let source_loc = error.source_loc as usize;
    
    // Calculate line and column from source position
    let (error_line, error_col) = calculate_line_col(source, source_loc);
    
    // Split source into lines
    let lines: Vec<&str> = source.lines().collect();
    
    // Calculate the range of lines to show
    let start_line = error_line.saturating_sub(context_lines);
    let end_line = std::cmp::min(error_line + context_lines + 1, lines.len());
    
    // Print error header
    eprintln!("Error: {}", error.message);
    eprintln!("  --> {}:{}:{}", filename, error_line + 1, error_col + 1);
    eprintln!();
    
    // Calculate the width needed for line numbers
    let line_number_width = (end_line + 1).to_string().len();
    
    // Print context lines
    for (i, line_content) in lines.iter().enumerate().take(end_line).skip(start_line) {
        let line_num = i + 1;
        let is_error_line = i == error_line;
        
        if is_error_line {
            // Print the error line with highlighting
            eprintln!("{:width$} | {}", line_num, line_content, width = line_number_width);
            
            // Print the error pointer
            let pointer_offset = error_col;
            let spaces = " ".repeat(line_number_width + 3 + pointer_offset); // +3 for " | "
            eprintln!("{}^", spaces);
        } else {
            // Print regular context line
            eprintln!("{:width$} | {}", line_num, line_content, width = line_number_width);
        }
    }
    eprintln!();
}

/// Calculate line and column position from byte offset in source
fn calculate_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 0;
    let mut col = 0;
    
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    
    (line, col)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        process::exit(1);
    }
    
    let source_file = &args[1];
    
    // Read the source file
    let source_code = match fs::read_to_string(source_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", source_file, e);
            process::exit(1);
        }
    };
    
    // Parse the source code with token preservation
    let mut lexer = Lexer::new(&source_code);
    let tokens = lexer.tokenize();
    
    let ast = match Parser::parse(&source_code) {
        Ok(ast) => ast,
        Err(e) => {
            print_error_with_context(&e, &source_code, source_file, 2);
            process::exit(1);
        }
    };
    
    // Format the code with comment/formatting preservation
    let formatter = AstPrinter::new_reformat(&ast, &tokens, &source_code, 4);
    let formatted_code = formatter.print();
    
    // Output the formatted code
    print!("{}", formatted_code);
}
