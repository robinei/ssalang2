use ssalang2::parse::parse_program;

fn main() {
    let input = r#"
        fn fibonacci(n: i32) -> i32 {
            if n == 0 {
                return 0;
            } else {
                if n == 1 {
                    return 1;
                } else {
                    return n + 42;
                }
            }
        }
    "#;
    
    match parse_program(input) {
        Ok(ast) => {
            println!("✅ Parsing successful!");
            if let Some(root) = ast.get_root() {
                match ast.get_node(root) {
                    ssalang2::ast::Node::Func(flags, locals_ref, _body, _return_type) => {
                        println!("📋 Function parsed:");
                        println!("  - Static: {}", flags.is_static());
                        println!("  - Inline: {}", flags.is_inline());
                        println!("  - Local count: {}", locals_ref.count);
                        
                        if locals_ref.count > 0 {
                            let locals = ast.get_locals(*locals_ref);
                            for (i, local) in locals.iter().enumerate() {
                                println!("  - Param {}: param={}, static={}, const={}", 
                                    i, local.is_param, local.is_static, local.is_const);
                            }
                        }
                    }
                    _ => println!("❌ Root is not a function"),
                }
            } else {
                println!("❌ No root node found");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {} at location {}", e.message, e.source_loc);
        }
    }
}