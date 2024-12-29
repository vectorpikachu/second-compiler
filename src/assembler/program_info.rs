use std::collections::HashMap;

use koopa::ir::{entities::ValueData, Program, Type, Value};

use super::function_info::FunctionInfo;

pub struct ProgramInfo<'a> {
    program: &'a Program,
    // 因为很难在 program 里找到变量的名字
    // 所以直接保存下来
    values: HashMap<Value, String>,
    current_func: Option<FunctionInfo>,
}

impl<'a> ProgramInfo<'a> {
    /// 创建一个 ProgramInfo
    pub fn new(program: &'a Program) -> Self {
        ProgramInfo {
            program,
            values: HashMap::new(),
            current_func: None,
        }
    }

    /// 获取当前实例的程序引用
    ///
    /// 此方法允许外部访问实例内部的 `Program` 对象，但不转移所有权或允许修改该对象
    /// 它提供了一种安全的方式来共享程序实例，而不会暴露其内部状态的风险
    ///
    /// # 返回值
    /// 返回一个 `Program` 类型的引用，该引用指向实例内部的程序对象
    /// 我改变这个的生命周期为 'a
    pub fn get_program(&self) -> &'a Program {
        self.program
    }

    pub fn get_value_name(&self, value: Value) -> Option<String> {
        self.values.get(&value).cloned()
    }

    pub fn get_current_func(&self) -> Option<&FunctionInfo> {
        self.current_func.as_ref()
    }

    pub fn get_current_func_mut(&mut self) -> Option<&mut FunctionInfo> {
        self.current_func.as_mut()
    }

    pub fn set_current_func(&mut self, func: FunctionInfo) {
        self.current_func = Some(func);
    }

    pub fn insert_value_name(&mut self, value: Value, name: String) {
        self.values.insert(value, name);
    }

    pub fn get_value_type(&self, value: Value) -> Type {
        if value.is_global() {
            return self.program.borrow_value(value).ty().clone();
        } else {
            return self
                .program
                .func(self.current_func.as_ref().unwrap().get_func().clone())
                .dfg()
                .value(value)
                .ty()
                .clone();
        }
    }
}
