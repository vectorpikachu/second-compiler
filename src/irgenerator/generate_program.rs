//! 生成内存形式的IR代码
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, FunctionData, Program, Type, Value};

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
        /* 一个函数被首先分解为:
         * %entry, [other blocks], %exit
         * 每个 block 之间通过 jump 连接
         * 所有的返回值都首先被分配到一个堆的变量 %retval 里
         */
        let func_ty = self.ty.generate_program(program, scopes);
        // TODO: Params Type
        let mut func_data = FunctionData::new(format!("@{}", self.ident), Vec::new(), func_ty);
        
        // 接下来, 生成函数的入口块
        let entry_block = func_data.dfg_mut().new_bb().basic_block(Some("%entry".to_string()));
        let exit_block = func_data.dfg_mut().new_bb().basic_block(Some("%exit".to_string()));
        let cur_block = func_data.dfg_mut().new_bb().basic_block(None);
        let mut ret: Option<Value> = None;
        if self.ty == FuncType::Int {
            // 直接把所有返回值扔到一个变量里面
            let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
            func_data.dfg_mut().set_value_name(alloc, Some("%retval".to_string()));
            ret = Some(alloc);
        }
        
        
        // Add function to program
        let func = program.new_func(func_data);

        /*
         * 处理完了Program里的函数, 接下来处理函数内部的内容
         * 也就是这样一个顺序: Progrma - Function - BasicBlock - Instruction
         */
        let mut function_info = FunctionInfo::new(func, entry_block, exit_block, ret);
        function_info.push_block(program, entry_block); // 更新 Layout
        if let Some(ret_inst) = function_info.get_return_value() {
            // 这是一个有返回值的函数
            function_info.push_inst(program, ret_inst);
        }
        // 所有的entry都会跳入这个当前的块中, 它因为没有名字
        // 所以会和entry相连接
        function_info.push_block(program, cur_block); // 更新 Layout

        scopes.enter_scope();
        // TODO: Params
        // 我们创建了这个函数的作用域, 应当把它的名字等放入
        scopes.add_func(&self.ident, func);
        scopes.set_current_func(function_info);
        self.block.generate_program(program, scopes);
        scopes.exit_scope();
        // 那么结束后必然进入了 exit_block, 此步要做的是
        let exit_block_info = scopes.get_current_func_mut().unwrap();
        // 指挥 entry_block 跳转到 我们的那个没名字的块中
        exit_block_info.make_entry_jump(program, cur_block);
        exit_block_info.make_function_exit(program); // 随后为这个函数收尾
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
                return_generate_program(ret_val, program, scopes);
            }
            _ => {}
        }
    }
}

pub fn return_generate_program<'a>(ret_val: &Option<Exp>, program: &mut Program, scopes: &mut Scopes<'a>) {
    match ret_val {
        Some(exp) => {
            let value = exp.generate_program(program, scopes);
            let func_info = scopes.get_current_func_mut().unwrap();
            let ret_inst: Value;
            let ret_val = func_info.get_return_value().unwrap();
            match value {
                ExpResult::Int(x) => {
                    ret_inst = func_info.new_value(program).store(x, ret_val);
                    // 当前这个实际上是对 Return 语句求值了
                    // 所以要把这个值放到 %retval 中
                    func_info.push_inst(program, ret_inst);
                }
                _ => {}
            }
        }
        None => {
            // 这是一个没有返回值的函数
        }
    }
    let func_info = scopes.get_current_func_mut().unwrap();
    // 接下来, 要把这个块跳转到 exit_block
    let exit_block = func_info.get_exit_block();
    let jump_inst = func_info.new_value(program).jump(*exit_block);
    func_info.push_inst(program, jump_inst);

    // 放入一个新的块
    let new_block = func_info.new_bb_dfg(program, None);
    func_info.push_block(program, new_block);
}

impl Exp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        self.lor_exp.generate_program(program, scopes)
    }
}

impl LOrExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            LOrExp::And(and_exp) => and_exp.generate_program(program, scopes),
            LOrExp::Or(left, _, right) => {
                ExpResult::Void
            }
        }
    }
}

impl LAndExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.generate_program(program, scopes),
            LAndExp::And(left, _, right) => {
                ExpResult::Void
            }
        }
    }
}

impl EqExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            EqExp::Rel(rel_exp) => rel_exp.generate_program(program, scopes),
            EqExp::Eq(left, op, right) => {
                let left_value = left.generate_program(program, scopes);
                let right_value = right.generate_program(program, scopes);
                match op {
                    EqOp::Eq => {
                        ExpResult::Void
                    }
                    EqOp::Ne => {
                        ExpResult::Void
                    }
                }
            }
        }
    }
}

impl RelExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            RelExp::Add(add_exp) => add_exp.generate_program(program, scopes),
            RelExp::Rel(left, op, right) => {
                let left_value = left.generate_program(program, scopes);
                let right_value = right.generate_program(program, scopes);
                match op {
                    RelOp::Lt => {
                        ExpResult::Void
                    }
                    RelOp::Gt => {
                        ExpResult::Void
                    }
                    RelOp::Le => {
                        ExpResult::Void
                    }
                    RelOp::Ge => {
                        ExpResult::Void
                    }
                }
            }
        }
    }
}

impl AddExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            AddExp::Mul(mul_exp) => mul_exp.generate_program(program, scopes),
            AddExp::Add(left, op, right) => {
                let left_value = left.generate_program(program, scopes);
                let right_value = right.generate_program(program, scopes);
                match op {
                    AddOp::Add => {
                        ExpResult::Void
                    }
                    AddOp::Sub => {
                        ExpResult::Void
                    }
                }
            }
        }
    }
}

impl MulExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            MulExp::Unary(unary_exp) => unary_exp.generate_program(program, scopes),
            MulExp::Mul(left, op, right) => {
                let left_value = left.generate_program(program, scopes);
                let right_value = right.generate_program(program, scopes);
                match op {
                    MulOp::Mul => {
                        ExpResult::Void
                    }
                    MulOp::Div => {
                        ExpResult::Void
                    }
                    MulOp::Mod => {
                        ExpResult::Void
                    }
                }
            }
        }
    }
}


impl UnaryExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            UnaryExp::Primary(primary_exp) => primary_exp.generate_program(program, scopes),
            UnaryExp::Unary(op, unary_exp) => {
                let value = unary_exp.generate_program(program, scopes);
                match op {
                    UnaryOp::Pos => {
                        ExpResult::Void
                    }
                    UnaryOp::Neg => {
                        ExpResult::Void
                    }
                    UnaryOp::Not => {
                        ExpResult::Void
                    }
                }
            }
            UnaryExp::Call(_, _) => {
                ExpResult::Void
            }
        }
    }
}

impl PrimaryExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            PrimaryExp::LVal(lval) => {
                lval.generate_program(program, scopes)
            }
            PrimaryExp::Number(x) => ExpResult::Int(
                scopes.get_current_func_mut().unwrap().new_value(program).integer(*x)
            ),
            PrimaryExp::Exp(exp) => exp.generate_program(program, scopes),
        }
    }
    
}

impl LVal {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        let var_value = scopes.get_value(&self.ident).cloned();
        match var_value {
            Some(VarValue::Const(x)) => {
                let func_info = scopes.get_current_func_mut().unwrap();
                ExpResult::Int(func_info.new_value(program).integer(x))
            }
            _ => {
                ExpResult::Void
            }
        }
    }
}
