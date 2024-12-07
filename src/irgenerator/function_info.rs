//! 用来记录当前的函数的信息

use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder}, BasicBlock, Function, Program, Value};

pub struct FunctionInfo {
    /// 当前的函数
    func: Function,
    /// 当前函数的入口
    entry_block: BasicBlock,
    /// 当前函数的出口
    exit_block: BasicBlock,
    /// 当前在的块
    current_block: BasicBlock,
    /// 返回值
    return_val: Option<Value>,
}

impl FunctionInfo {
    pub fn new(
        func: Function,
        entry_block: BasicBlock,
        exit_block: BasicBlock,
        return_value: Value,
    ) -> Self {
        FunctionInfo {
            func,
            entry_block,
            exit_block,
            current_block: entry_block,
            return_val: Some(return_value),
        }
    }

    /// 得到当前的函数
    pub fn get_func(&self) -> &Function {
        &self.func
    }

    /// 得到当前的函数的入口
    pub fn get_entry_block(&self) -> &BasicBlock {
        &self.entry_block
    }

    /// 得到当前的函数的出口
    pub fn get_exit_block(&self) -> &BasicBlock {
        &self.exit_block
    }

    /// 得到当前的块
    pub fn get_current_block(&self) -> &BasicBlock {
        &self.current_block
    }

    /// 把一个块推入函数
    pub fn push_block(&mut self, program: &mut Program, block: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .push_key_back(block)
            .unwrap();
        self.current_block = block;
    }

    /// 把一个指令推入块
    pub fn push_inst_to(&mut self, program: &mut Program, inst: Value, block: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    /// 把一个指令推入当前块
    pub fn push_inst(&mut self, program: &mut Program, inst: Value) {
        self.push_inst_to(program, inst, self.current_block);
    }

    /// 得到返回值
    pub fn get_return_value(&self) -> Option<Value> {
        self.return_val
    }

    /// 插入一个新的值
    pub fn new_store_value<'a>(&self, program: &'a mut Program, src: Value, dst: Value) -> Value {
        program.func_mut(self.func).dfg_mut().new_value().store(src, dst)
    }

    pub fn new_jump_value<'a>(&self, program: &'a mut Program, target: BasicBlock) -> Value {
        program.func_mut(self.func).dfg_mut().new_value().jump(target)
    }

    /// Creates a new basic block in function.
    pub fn new_bb(&self, program: &mut Program, name: Option<&str>) -> BasicBlock {
        program
        .func_mut(self.func)
        .dfg_mut()
        .new_bb()
        .basic_block(name.map(|s| s.into()))
    }

    /// 插入一个新的基本块节点
    pub fn new_bb_node(&self, program: &mut Program, block: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .push_key_back(block)
            .unwrap();
    }


}
