use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

#[derive(Debug, Clone)]
pub enum RealValue {
    Const(i32),
    StackPos(i32), // 后面是偏移
    Reg(String), // 寄存器
    DataSeg(String), // .data 上的变量
    None,
}

pub struct FunctionInfo {
    func: Function,
    bb_names: HashMap<BasicBlock, String>,
    now_id: usize,
    stack_size: usize,
    alloc_offset: HashMap<Value, i32>,
    current_offset: i32,
    real_value: HashMap<Value, RealValue>,
}

impl FunctionInfo {
    pub fn new(func: Function) -> Self {
        FunctionInfo {
            func,
            bb_names: HashMap::new(),
            now_id: 0,
            stack_size: 0,
            alloc_offset: HashMap::new(),
            current_offset: 0,
            real_value: HashMap::new(),
        }
    }

    pub fn get_func(&self) -> &Function {
        &self.func
    }

    pub fn get_bb_name(&self, bb: BasicBlock) -> String {
        self.bb_names.get(&bb).unwrap().clone()
    }

    pub fn set_bb_name(&mut self, bb: BasicBlock, name: Option<String>) {
        self.now_id += 1;
        match name {
            Some(name) => {
                // Koopa 里的名字是我们自己设置的名字
                // 打印出来自动加编号, 但是里面存储的话依然是原先的
                // 还是生成 risc-v 需要自己手动加编号
                self.bb_names.insert(bb, format!("{}{}", name.strip_prefix("%").unwrap().to_string(), self.now_id));
            },
            None => {
                self.bb_names.insert(bb, format!("bb{}", self.now_id));
            }
        }
    }

    pub fn get_stack_size(&self) -> usize {
        self.stack_size
    }

    pub fn set_stack_size(&mut self, size: usize) {
        self.stack_size = size;
    }

    pub fn get_current_offset(&mut self, value: &Value) -> i32 {
        match self.alloc_offset.get(value) {
            Some(offset) => *offset,
            None => {
                self.current_offset += 4;
                self.alloc_offset.insert(*value, self.current_offset);
                self.current_offset
            }
        }
    }

    pub fn get_allocs(&self) -> &HashMap<Value, i32> {
        &self.alloc_offset
    }

    pub fn get_real_value(&self, value: &Value) -> RealValue {
        match self.real_value.get(value) {
            Some(real_value) => real_value.clone(),
            None => RealValue::None,
        }
    }

    pub fn set_real_value(&mut self, value: Value, real_value: RealValue) {
        self.real_value.insert(value, real_value);
    }

}