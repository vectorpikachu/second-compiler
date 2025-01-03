use std::{collections::HashMap, fmt::Debug};

use koopa::ir::{BasicBlock, Function, Type, TypeKind, Value};

#[derive(Debug, Clone)]
pub enum RealValue {
    Const(i32),
    StackPos(i32), // 后面是偏移
    Pointer(i32), // 指针
    Reg(String), // 寄存器
    DataSeg(String), // .data 上的变量
    Array(i32),
    None,
}

pub struct FunctionInfo {
    func: Function,
    bb_names: HashMap<BasicBlock, String>,
    now_id: usize,
    stack_size: usize,
    call_flag: bool,
    inst_offset: HashMap<Value, i32>,
    alloc_offset: HashMap<Value, (i32, Type)>,
    current_offset: i32,
    all_types: HashMap<Value, Type>, // 存放需要的数组类型
}

impl FunctionInfo {
    pub fn new(func: Function) -> Self {
        FunctionInfo {
            func,
            bb_names: HashMap::new(),
            now_id: 0,
            stack_size: 0,
            call_flag: false,
            inst_offset: HashMap::new(),
            alloc_offset: HashMap::new(),
            current_offset: 0,
            all_types: HashMap::new(),
        }
    }

    pub fn get_func(&self) -> &Function {
        &self.func
    }

    pub fn get_bb_name(&self, bb: BasicBlock) -> String {
        self.bb_names.get(&bb).unwrap().clone()
    }

    pub fn set_bb_name(&mut self, bb: BasicBlock, name: Option<String>, func_name: String, func_id: i32) {
        self.now_id += 1;
        match name {
            Some(name) => {
                // Koopa 里的名字是我们自己设置的名字
                // 打印出来自动加编号, 但是里面存储的话依然是原先的
                // 还是生成 risc-v 需要自己手动加编号
                self.bb_names.insert(bb, format!("{}_{}_{}_{}", func_name, func_id, name.strip_prefix("%").unwrap().to_string(), self.now_id));
            },
            None => {
                self.bb_names.insert(bb, format!("{}_{}_bb_{}", func_name, func_id, self.now_id));
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
        match self.inst_offset.get(value) {
            Some(offset) => *offset,
            None => {
                self.inst_offset.insert(*value, self.current_offset);
                self.current_offset += 4;
                self.current_offset - 4
            }
        }
    }

    pub fn set_current_offset(&mut self, offset: i32) {
        self.current_offset = offset;
    }

    /// 返回 alloc 的偏移
    pub fn get_alloc_offset(&self, value: &Value) -> Option<i32> {
        match self.alloc_offset.get(value) {
            Some(offset) => Some(offset.0),
            None => None
        }
    }

    pub fn is_alloc_array(&self, value: &Value) -> bool {
       match self.alloc_offset.get(value) {
           Some(offset) => {
                match offset.1.kind() {
                    TypeKind::Array(_, _) => true,
                    _ => false,
                }
           }
           None => false
       }
    }

    /// 设置 alloc 的偏移
    pub fn set_alloc_offset(&mut self, value: Value, type_kind: TypeKind) {
        let size = match type_kind {
            TypeKind::Int32 => 4,
            TypeKind::Pointer(base) => {
                self.alloc_offset.insert(value, (self.current_offset, base.clone()));
                base.size()
            }
            _ => 4,
        };
        self.current_offset += size as i32;
    }

    /// 得到 alloc 的类型
    pub fn get_alloc_type_kind(&self, value: &Value) -> Option<&Type> {
        match self.alloc_offset.get(value) {
            Some(offset) => Some(&offset.1),
            None => None,
        }
    }

    pub fn insert_type(&mut self, value: Value, ty: Type) {
        self.all_types.insert(value, ty);
    }

    pub fn get_type(&self, value: &Value) -> Option<&Type> {
        self.all_types.get(value)
    }

    pub fn set_call_flag(&mut self, flag: bool) {
        self.call_flag = flag;
    }

    pub fn get_call_flag(&self) -> bool {
        self.call_flag
    }

}

