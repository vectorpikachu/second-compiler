use asm_struct::ToBuffer;
use koopa::ir::{Program, Type};

mod generate_asm;
mod program_info;
mod function_info;
mod asm_struct;
mod peephole_opt;

use generate_asm::GenerateAsm;
use peephole_opt::{parse_assembly, PeepholeOpt};

pub fn generate_riscv(program: &Program, buf: &mut Vec<u8>) {
    /*
     为了适配 riscv32 的指针宽度, 
     你需要在进行代码生成前 (比如在 main 里), 
     调用 Type::set_ptr_size(4), 来设置指针类型的大小为 4 字节.
     */
    Type::set_ptr_size(4);
    let mut program_info = program_info::ProgramInfo::new(program);
    program.generate_asm(buf, &mut program_info);
    generate_asm_program(buf);
}

pub fn generate_asm_program(buf: &mut Vec<u8>) {
    let code = String::from_utf8(buf.clone()).unwrap();
    let asm_program = parse_assembly(code.as_str());
    let new_program = asm_program.peephole_opt();
    buf.clear();
    new_program.to_buffer(buf);
}