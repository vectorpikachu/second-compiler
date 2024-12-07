//! 生成内存形式的IR代码
use core::alloc;

use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, Function, FunctionData, Program, Type, Value};

use crate::ast::*;
use super::{expression::ExpResult, function_info::FunctionInfo, scopes::*};

impl CompUnit {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        for entry in &self.entries {
            entry.generate_program(program, scopes);
        }
    }
}

impl GlobalEntry {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        match self {
            GlobalEntry::Decl(decl) => decl.generate_program(program, scopes),
            GlobalEntry::FuncDef(func_def) => func_def.generate_program(program, scopes),
        }
    }
}

impl Decl {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        match self {
            Decl::Var(var_decl) => var_decl.generate_program(program, scopes),
            Decl::Const(const_decl) => const_decl.generate_program(program, scopes),
        }
    }
}

impl ConstDecl {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 现阶段都是int类型的常量
        // self.ty = BType::Int;
        for def in &self.defs {
            def.generate_program(program, scopes);
        }
    }
}

impl ConstDef {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 首先, 这个常量的类型可能只是单纯的int, 也可能是数组
        // 此时应该处理数组的情况
        // 暂时先不处理
        let value = self.init_val.generate_program(program, scopes);
        scopes.set_value(&self.ident, VarValue::Const(value));
    }
}

impl ConstInitVal {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> i32 {
        match self {
            ConstInitVal::Exp(exp) => exp.evaluate(scopes),
            ConstInitVal::Array(array) => {
                let mut values = Vec::new();
                for val in array {
                    values.push(val.generate_program(program, scopes));
                }
                // 暂时先不处理数组
                0
            }
        }
    }
    
}

impl VarDecl {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 现阶段都是int类型的变量
        // self.ty = BType::Int;
        for def in &self.defs {
            def.generate_program(program, scopes);
        }
    }
}

impl VarDef {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 首先, 这个变量的类型可能只是单纯的int, 也可能是数组
        // 此时应该处理数组的情况
        // 暂时先不处理
        // 生成一个alloc指令
        // TODO: Do it later.
    }
}

impl FuncDef {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 读入一个函数的定义
        // ty, ident, params, block
        let func_ty = self.ty.generate_program(program, scopes);
        // TODO: Params Type
        let mut func_data = FunctionData::new(format!("@{}", self.ident), Vec::new(), func_ty);
        
        // 接下来, 生成函数的入口块
        let entry_block = func_data.dfg_mut().new_bb().basic_block(Some("%entry".to_string()));
        let mut ret: Option<Value> = None;
        if self.ty == FuncType::Int {
            // 直接把所有返回值扔到一个变量里面
            let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
            func_data.dfg_mut().set_value_name(alloc, Some("%retval".to_string()));
            ret = Some(alloc);
        }
        

        // Add function to program
        let func = program.new_func(func_data);
    }
}

impl FuncType {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) -> Type {
        match self {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        }
    }
}

impl Block {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        scopes.enter_scope();
        for item in &self.items {
            item.generate_program(program, scopes);
        }
        scopes.exit_scope();
    }
}

impl BlockItem {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        match self {
            BlockItem::Stmt(stmt) => stmt.generate_program(program, scopes),
            BlockItem::Decl(decl) => decl.generate_program(program, scopes),
        }
    }
}

impl Stmt {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        match self {
            Stmt::Return(ret_val) => {
                match ret_val {
                    Some(x) => {
                        println!("Return value: {:?}", &x);
                        let value = x.generate_program(program, scopes);
                        let current_func = &mut scopes.get_current_func_mut().unwrap();
                        let return_val = current_func.get_return_value();
                        let new_value = program.func_mut(*current_func.get_func()).dfg_mut().new_value().integer(0);
                        let store_value = current_func.new_store_value(program, new_value, return_val.unwrap());
                        current_func.push_inst(program, store_value);

                        let target_block = current_func.get_exit_block();
                        current_func.new_jump_value(program, *target_block);
                        current_func.push_block(program, *target_block);
                        let next_block = current_func.new_bb(program, None);
                        current_func.push_block(program, next_block);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}

impl Exp {
    pub fn generate_program(&self, program: &mut Program, scopes: &Scopes) -> ExpResult {
        // self.lor_exp.generate_program(program, scopes);
        ExpResult::Void
    }
}

