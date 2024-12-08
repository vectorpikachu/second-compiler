//! 作用域管理

use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

use super::function_info::FunctionInfo;


#[derive(Debug, Clone)]
pub enum VarValue {
    Value(Value), // 一个变量
    Const(i32), // 一个常量
}

/// 当前的作用域
pub struct Scopes<'a> {
    values: Vec<HashMap<&'a str, VarValue>>, // 值的符号表
    funcs: HashMap<&'a str, Function>,
    current_func: Option<FunctionInfo>,
    loop_stack: Vec<(BasicBlock, BasicBlock)>, // 进入的点和退出的点
}

impl<'a> Scopes<'a> {
    pub fn new() -> Self {
        Scopes {
            values: vec![HashMap::new()],
            funcs: HashMap::new(),
            current_func: None,
            loop_stack: Vec::new(),
        }
    }

    /// 进入一个新的作用域
    pub fn enter_scope(&mut self) {
        self.values.push(HashMap::new());
    }

    /// 退出一个作用域
    pub fn exit_scope(&mut self) {
        self.values.pop();
    }

    /// 得到当前的作用域中的一个值
    pub fn get_value(&self, name: &str) -> Option<&VarValue> {
        for scope in self.values.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// 设置当前作用域中的一个值
    pub fn set_value(&mut self, name: &'a str, value: VarValue) {
        self.values.last_mut().unwrap().insert(name, value);
    }

    /// 得到当前的函数
    pub fn get_current_func(&self) -> Option<&FunctionInfo> {
        self.current_func.as_ref()
    }

    /// 得到当前的可变函数
    pub fn get_current_func_mut(&mut self) -> Option<&mut FunctionInfo> {
        self.current_func.as_mut()
    }

    /// 设置当前的函数
    pub fn set_current_func(&mut self, func: FunctionInfo) {
        self.current_func = Some(func);
    }

    /// 得到当前的循环
    pub fn get_current_loop(&self) -> Option<&(BasicBlock, BasicBlock)> {
        self.loop_stack.last()
    }

    pub fn get_current_loop_mut(&mut self) -> Option<&(BasicBlock, BasicBlock)> {
        self.loop_stack.last()
    }

    /// 进入一个循环
    pub fn enter_loop(&mut self, enter: BasicBlock, exit: BasicBlock) {
        self.loop_stack.push((enter, exit));
    }

    /// 退出一个循环
    pub fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    /// 得到一个函数
    pub fn get_func(&self, name: &str) -> Option<&Function> {
        self.funcs.get(name)
    }

    /// 判断当前是否是全局作用域
    pub fn is_global_scope(&self) -> bool {
        self.current_func.is_none()
    }

    /// 添加一个函数
    pub fn add_func(&mut self, name: &'a str, func: Function) {
        self.funcs.insert(name, func);
    }

}