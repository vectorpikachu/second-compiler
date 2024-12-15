use koopa::ir::Program;

mod generate_asm;
mod program_info;
mod function_info;

use generate_asm::GenerateAsm;

pub fn generate_riscv(program: &Program, buf: &mut Vec<u8>) {
    let mut program_info = program_info::ProgramInfo::new(program);
    program.generate_asm(buf, &mut program_info);
}