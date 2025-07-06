use crate::code::Code;
use crate::ir::{Instr, InstrRef, BlockRef, PhiRef, Type, Meta};
use std::fmt::Write;

/// Printer for IR code that produces human-readable assembly-like output
pub struct IrPrinter<'a> {
    code: &'a Code,
    use_colors: bool,
    buffer: String,
}

#[derive(Clone, Copy)]
enum Color {
    Reset,
    Instruction,  // Blue
    Reference,    // Green  
    Type,         // Yellow
    Constant,     // Cyan
    Comment,      // Gray
}

impl Color {
    fn as_ansi(self) -> &'static str {
        match self {
            Color::Reset => "\x1b[0m",
            Color::Instruction => "\x1b[34m",  // Blue
            Color::Reference => "\x1b[32m",    // Green
            Color::Type => "\x1b[33m",         // Yellow
            Color::Constant => "\x1b[36m",     // Cyan
            Color::Comment => "\x1b[90m",      // Gray
        }
    }
}

impl<'a> IrPrinter<'a> {
    /// Create a new IR printer without colors
    pub fn new(code: &'a Code) -> Self {
        Self {
            code,
            use_colors: false,
            buffer: String::with_capacity(1024),
        }
    }

    /// Create a new IR printer with ANSI colors
    pub fn new_colored(code: &'a Code) -> Self {
        Self {
            code,
            use_colors: true,
            buffer: String::with_capacity(1024),
        }
    }

    /// Print the IR code and return the formatted string
    pub fn print(mut self) -> String {
        self.buffer.clear();
        
        self.print_constants();
        self.print_scheduled_code();
        
        self.buffer
    }

    fn print_constants(&mut self) {
        let mut has_constants = false;
        
        // Check if we have any negative indices (constants)
        for (instr_ref, _) in self.code.iter_with_refs() {
            if instr_ref.get() < 0 {
                has_constants = true;
                break;
            }
        }
        
        if !has_constants {
            return;
        }
        
        self.write_colored(Color::Comment, "; === CONSTANTS & UNSCHEDULED ===\n");
        
        for (instr_ref, instr) in self.code.iter_with_refs() {
            if instr_ref.get() < 0 {
                self.print_constant_instruction(instr_ref, instr);
            }
        }
        
        self.buffer.push('\n');
    }

    fn print_scheduled_code(&mut self) {
        let mut has_scheduled = false;
        
        // Check if we have any positive indices (scheduled instructions)
        for (instr_ref, _) in self.code.iter_with_refs() {
            if instr_ref.get() > 0 {
                has_scheduled = true;
                break;
            }
        }
        
        if !has_scheduled {
            return;
        }
        
        self.write_colored(Color::Comment, "; === FUNCTION CODE ===\n");
        
        for (instr_ref, instr) in self.code.iter_with_refs() {
            if instr_ref.get() > 0 {
                match instr {
                    Instr::Label(_, block_ref) => {
                        // Print block header comment, don't print the label instruction
                        let block_name = self.format_block_ref(*block_ref);
                        write!(&mut self.buffer, "; Block {}:\n", block_name).unwrap();
                    }
                    _ => {
                        self.print_scheduled_instruction(instr_ref, instr);
                    }
                }
            }
        }
    }

    fn print_constant_instruction(&mut self, instr_ref: InstrRef, instr: &Instr) {
        // Format: "c1:    const_i32  42        ; i32"
        let const_label = self.format_const_ref(instr_ref);
        self.write_colored(Color::Reference, &format!("{}:", const_label));
        self.buffer.push_str("    ");
        
        self.print_instruction_body(instr);
        self.print_type_comment(instr.get_meta());
        self.buffer.push('\n');
    }

    fn print_scheduled_instruction(&mut self, instr_ref: InstrRef, instr: &Instr) {
        // Only print instruction label for non-void instructions
        if instr.get_type() != Type::Void {
            let instr_label = self.format_instr_ref(instr_ref);
            self.write_colored(Color::Reference, &format!("{}:", instr_label));
            self.buffer.push_str("    ");
        } else {
            // Extra spaces to align with labeled instructions
            self.buffer.push_str("       ");
        }
        
        self.print_instruction_body(instr);
        
        // Only print type comment for non-void
        if instr.get_type() != Type::Void {
            self.print_type_comment(instr.get_meta());
        }
        
        self.buffer.push('\n');
    }

    fn print_instruction_body(&mut self, instr: &Instr) {
        match instr {
            Instr::Nop(_) => {
                self.write_colored(Color::Instruction, "nop");
            }
            Instr::Identity(_, operand) => {
                self.write_colored(Color::Instruction, "identity");
                self.buffer.push_str("  ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*operand));
            }
            Instr::Print(_, operand) => {
                self.write_colored(Color::Instruction, "print");
                self.buffer.push_str("      ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*operand));
            }
            Instr::Label(_, block_ref) => {
                self.write_colored(Color::Instruction, "label");
                self.buffer.push_str("      ");
                self.write_colored(Color::Reference, &self.format_block_ref(*block_ref));
            }
            Instr::Jump(_, block_ref) => {
                self.write_colored(Color::Instruction, "jump");
                self.buffer.push_str("       ");
                self.write_colored(Color::Reference, &self.format_block_ref(*block_ref));
            }
            Instr::Branch(_, cond, true_block, false_block) => {
                self.write_colored(Color::Instruction, "branch");
                self.buffer.push_str("     ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*cond));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_block_ref(*true_block));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_block_ref(*false_block));
            }
            Instr::Ret(_, value) => {
                self.write_colored(Color::Instruction, "ret");
                self.buffer.push_str("        ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*value));
            }
            Instr::Upsilon(_, phi_ref, value) => {
                self.write_colored(Color::Instruction, "upsilon");
                self.buffer.push_str("    ");
                self.write_colored(Color::Reference, &self.format_phi_ref(*phi_ref));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*value));
            }
            Instr::Phi(_, phi_ref) => {
                self.write_colored(Color::Instruction, "phi");
                self.buffer.push_str("        ");
                self.write_colored(Color::Reference, &self.format_phi_ref(*phi_ref));
            }
            Instr::ConstBool(_, value) => {
                self.write_colored(Color::Instruction, "const_bool");
                self.buffer.push_str(" ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstI32(_, value) => {
                self.write_colored(Color::Instruction, "const_i32");
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::Arg(_, index) => {
                self.write_colored(Color::Instruction, "arg");
                self.buffer.push_str("        ");
                self.write_colored(Color::Constant, &index.to_string());
            }
            Instr::Add(_, left, right) => {
                self.write_colored(Color::Instruction, "add");
                self.buffer.push_str("        ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*left));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*right));
            }
            Instr::Eq(_, left, right) => {
                self.write_colored(Color::Instruction, "eq");
                self.buffer.push_str("         ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*left));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*right));
            }
            Instr::Neq(_, left, right) => {
                self.write_colored(Color::Instruction, "neq");
                self.buffer.push_str("        ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*left));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*right));
            }
        }
    }

    fn print_type_comment(&mut self, meta: Meta) {
        let type_str = match meta.get_type() {
            Type::Void => "void",
            Type::Bool => "bool", 
            Type::I32 => "i32",
        };
        
        // Add padding to align comments
        let padding = 20_i32.saturating_sub(self.buffer.len() as i32 % 100); // Rough alignment
        for _ in 0..padding.max(2) {
            self.buffer.push(' ');
        }
        
        self.write_colored(Color::Comment, "; ");
        self.write_colored(Color::Type, type_str);
    }

    fn format_const_ref(&self, instr_ref: InstrRef) -> String {
        let index = instr_ref.get().abs() as u32;
        format!("c{}", index)
    }

    fn format_instr_ref(&self, instr_ref: InstrRef) -> String {
        let index = instr_ref.get();
        if index < 0 {
            self.format_const_ref(instr_ref)
        } else {
            format!("i{}", index)
        }
    }

    fn format_block_ref(&self, block_ref: BlockRef) -> String {
        format!("b{}", block_ref.get().abs())
    }

    fn format_phi_ref(&self, phi_ref: PhiRef) -> String {
        format!("p{}", phi_ref.get().abs())
    }

    fn write_colored(&mut self, color: Color, text: &str) {
        if self.use_colors {
            self.buffer.push_str(color.as_ansi());
            self.buffer.push_str(text);
            self.buffer.push_str(Color::Reset.as_ansi());
        } else {
            self.buffer.push_str(text);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::*;

    #[test]
    fn test_empty_code() {
        let code = Code::new();
        let printer = IrPrinter::new(&code);
        let output = printer.print();
        
        insta::assert_snapshot!(output, @"");
    }

    #[test]
    fn test_constants_only() {
        let mut code = Code::new();
        code.push_unpinned(Instr::const_i32(42));
        code.push_unpinned(Instr::const_bool(true));
        
        let printer = IrPrinter::new(&code);
        let output = printer.print();
        
        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c2:    const_bool true  ; bool
        c1:    const_i32  42  ; i32

        "###);
    }

    #[test]
    fn test_scheduled_instructions() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_i32(42));
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(1).unwrap()));
        code.push_pinned(Instr::Print(Meta::new(Type::Void), const_ref));
        code.push_pinned(Instr::Ret(Meta::new(Type::Void), const_ref));
        
        let printer = IrPrinter::new(&code);
        let output = printer.print();
        
        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
               print      c1
               ret        c1
        "###);
    }

    #[test] 
    fn test_non_void_instruction_labeling() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_i32(42));
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(1).unwrap()));
        let add_ref = code.push_pinned(Instr::Add(Meta::new(Type::I32), const_ref, const_ref));
        code.push_pinned(Instr::Ret(Meta::new(Type::Void), add_ref));
        
        let printer = IrPrinter::new(&code);
        let output = printer.print();
        
        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
        i2:    add        c1, c1  ; i32
               ret        i2
        "###);
    }

    #[test]
    fn test_comprehensive_example() {
        let mut code = Code::new();
        
        // Constants
        let const42 = code.push_unpinned(Instr::const_i32(42));
        let _const_true = code.push_unpinned(Instr::const_bool(true));
        let const10 = code.push_unpinned(Instr::const_i32(10));
        
        // Function with multiple blocks
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(1).unwrap()));
        let add_result = code.push_pinned(Instr::Add(Meta::new(Type::I32), const42, const10));
        let eq_result = code.push_pinned(Instr::Eq(Meta::new(Type::Bool), add_result, const42));
        code.push_pinned(Instr::Branch(
            Meta::new(Type::Void), 
            eq_result, 
            BlockRef::new(2).unwrap(), 
            BlockRef::new(3).unwrap()
        ));
        
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(2).unwrap()));
        code.push_pinned(Instr::Print(Meta::new(Type::Void), const42));
        code.push_pinned(Instr::Ret(Meta::new(Type::Void), const42));
        
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(3).unwrap()));
        code.push_pinned(Instr::Ret(Meta::new(Type::Void), const10));
        
        let printer = IrPrinter::new(&code);
        let output = printer.print();
        
        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c3:    const_i32  10  ; i32
        c2:    const_bool true  ; bool
        c1:    const_i32  42       ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
        i2:    add        c1, c3  ; i32
        i3:    eq         i2, c1  ; bool
               branch     i3, b2, b3
        ; Block b2:
               print      c1
               ret        c1
        ; Block b3:
               ret        c3
        "###);
    }

    #[test]
    fn test_colored_output() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_i32(42));
        code.push_pinned(Instr::Label(Meta::new(Type::Void), BlockRef::new(1).unwrap()));
        code.push_pinned(Instr::Ret(Meta::new(Type::Void), const_ref));
        
        let printer = IrPrinter::new_colored(&code);
        let output = printer.print();
        
        // Should contain ANSI color codes
        assert!(output.contains("\x1b["));
        assert!(output.contains("const_i32"));
        assert!(output.contains("ret"));
        
        // Test structure without colors using snapshot
        let uncolored_printer = IrPrinter::new(&code);
        let uncolored_output = uncolored_printer.print();
        
        insta::assert_snapshot!(uncolored_output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
               ret        c1
        "###);
    }
}