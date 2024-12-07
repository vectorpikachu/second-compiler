use koopa::ir::Program;
use crate::ast::*;

mod generate_program;
mod scopes;
mod expression;
mod function_info;
/// 生成IR的入口
pub fn generate_program(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program = Program::new();
    let mut scopes = scopes::Scopes::new(); // 这个相当于全局作用域
    comp_unit.generate_program(&mut program, &mut scopes);
    Ok(program)
}