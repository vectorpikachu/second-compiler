use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function};


pub struct FunctionInfo {
    func: Function,
    bb_names: HashMap<BasicBlock, String>,
    now_id: usize,
}

impl FunctionInfo {
    pub fn new(func: Function) -> Self {
        FunctionInfo {
            func,
            bb_names: HashMap::new(),
            now_id: 0,
        }
    }

    pub fn get_func(&self) -> &Function {
        &self.func
    }

    pub fn get_bb_name(&self, bb: BasicBlock) -> String {
        self.bb_names.get(&bb).unwrap().clone()
    }

    pub fn set_bb_name(&mut self, bb: BasicBlock, name: Option<String>) {
        match name {
            Some(name) => {
                self.bb_names.insert(bb, name.strip_prefix("%").unwrap().to_string());
            },
            None => {
                self.now_id += 1;
                self.bb_names.insert(bb, format!("bb{}", self.now_id));
            }
        }
    }
}