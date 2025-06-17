use ssalang2::parse::parse_program;
use ssalang2::pretty_print::PrettyPrinter;

fn main() {
    let inputs = vec![
        "fn main() { if true { return 1; } }",
        "fn main() { if false { return 1; } else { return 2; } }",
        "fn main() { if local_0 == 1 { return 1; } else if local_0 == 2 { return 2; } else { return 0; } }",
        "fn main() { while true { break; } }",
        "fn main() { while true { if local_0 == 5 { break; } else { continue; } } }",
    ];
    
    for (i, input) in inputs.iter().enumerate() {
        println!("=== Test {} ===", i + 1);
        println!("Input: {}", input);
        let ast = parse_program(input).unwrap();
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        println!("Output: {:?}", output);
        println!();
    }
}