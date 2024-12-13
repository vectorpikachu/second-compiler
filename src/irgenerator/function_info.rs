//! 用来记录当前的函数的信息

use koopa::ir::{
    builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder}, BasicBlock, Function, Program, Type, Value
};

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
        return_value: Option<Value>,
    ) -> Self {
        FunctionInfo {
            func,
            entry_block,
            exit_block,
            current_block: entry_block,
            return_val: return_value,
        }
    }

    /// 得到当前的函数
    pub fn get_func(&self) -> &Function {
        &self.func
    }


    /// 得到当前的函数的出口
    pub fn get_exit_block(&self) -> &BasicBlock {
        &self.exit_block
    }


    /// 把一个块推入函数的 Layout 中
    pub fn push_block(&mut self, program: &mut Program, block: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .push_key_back(block)
            .unwrap();
        self.current_block = block;
    }

    /// 把一个指令推入某个块中
    pub fn push_inst_to(&self, program: &mut Program, inst: Value, block: BasicBlock) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    /// 把一个指令推入当前块
    pub fn push_inst(&self, program: &mut Program, inst: Value) {
        self.push_inst_to(program, inst, self.current_block);
    }

    /// 得到返回值
    pub fn get_return_value(&self) -> Option<Value> {
        self.return_val
    }

    /// 创建一个新的值 / 指令
    /// 为每个Value的种类写一个太麻烦了, 直接用它上面的哪一个
    pub fn new_value<'a>(&self, program: &'a mut Program) -> LocalBuilder<'a> {
        program.func_mut(self.func).dfg_mut().new_value()
    }

    /// 为这个函数创建一个新的基本块
    /// 首先在 DFG 中创建一个新的基本块
    pub fn new_bb_dfg(&self, program: &mut Program, name: Option<&str>) -> BasicBlock {
        program
            .func_mut(self.func)
            .dfg_mut()
            .new_bb()
            .basic_block(name.map(|s| s.into()))
    }

    /// 指挥 entry_block 跳转到 我们的那个没名字的块中
    pub fn make_entry_jump(&mut self, program: &mut Program, target: BasicBlock) {
        let jump = self.new_value(program).jump(target);
        self.push_inst_to(program, jump, self.entry_block);
    }

    /// 给函数收尾
    pub fn make_function_exit(&mut self, program: &mut Program) {
        let jump = self.new_value(program).jump(self.exit_block);
        self.push_inst_to(program, jump, self.current_block);
        // 接着把 exit_block 放入 Layout 中
        self.push_block(program, self.exit_block);

        // 为 exit_block 添加一个返回值
        // self.return_val 是一个 Alloc 指令, 我们从中Load
        if let Some(ret) = self.return_val {
            let load = self.new_value(program).load(ret);
            self.push_inst_to(program, load, self.exit_block);
            let ret_inst = self.new_value(program).ret(Some(load));
            self.push_inst_to(program, ret_inst, self.exit_block);
        } else {
            let ret_inst = self.new_value(program).ret(None);
            self.push_inst_to(program, ret_inst, self.exit_block);
        }
    }

    /// 获取一个新的 Alloc 指令
    /// 而且加入 entry block
    pub fn new_alloc_entry(&mut self, program: &mut Program, ty: Type, name: Option<&str>) -> Value {
        let alloc = self.new_value(program).alloc(ty);
        if let Some(name) = name {
            program
                .func_mut(self.func)
                .dfg_mut()
                .set_value_name(alloc, Some(format!("@{}", name)));
        }
        self.push_inst_to(program, alloc, self.entry_block);
        alloc
    }

}
