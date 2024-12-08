//! 生成内存形式的IR代码

use koopa::ir::{
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type, Value, ValueKind,
};

use super::{expression::ExpResult, function_info::FunctionInfo, scopes::*};
use crate::ast::*;

impl CompUnit {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 应该声明所有的库函数
        scopes.add_func(
            "getint",
            program.new_func(FunctionData::new_decl(
                "@getint".to_string(),
                Vec::new(),
                Type::get_i32(),
            )),
        );
        scopes.add_func(
            "getch",
            program.new_func(FunctionData::new_decl(
                "@getch".to_string(),
                Vec::new(),
                Type::get_i32(),
            )),
        );
        scopes.add_func(
            "getarray",
            program.new_func(FunctionData::new_decl(
                "@getint".to_string(),
                vec![Type::get_pointer(Type::get_i32())],
                Type::get_i32(),
            )),
        );
        scopes.add_func(
            "putint",
            program.new_func(FunctionData::new_decl(
                "@putint".to_string(),
                vec![Type::get_i32()],
                Type::get_unit(),
            )),
        );
        scopes.add_func(
            "putch",
            program.new_func(FunctionData::new_decl(
                "@putch".to_string(),
                vec![Type::get_i32()],
                Type::get_unit(),
            )),
        );
        scopes.add_func(
            "putarray",
            program.new_func(FunctionData::new_decl(
                "@putarray".to_string(),
                vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
                Type::get_unit(),
            )),
        );
        scopes.add_func(
            "starttime",
            program.new_func(FunctionData::new_decl(
                "@starttime".to_string(),
                Vec::new(),
                Type::get_unit(),
            )),
        );
        scopes.add_func(
            "stoptime",
            program.new_func(FunctionData::new_decl(
                "@stoptime".to_string(),
                Vec::new(),
                Type::get_unit(),
            )),
        );
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
        /*
         * 根据是否是全局变量, 生成不同的代码
         */
        if scopes.is_global_scope() {
            match self.init_val {
                Some(ref init_val) => {
                    let value = init_val.evaluate(scopes);
                    let init_val = program.new_value().integer(value);
                    let global_value = program.new_value().global_alloc(init_val);
                    program.set_value_name(global_value, Some(format!("@{}", self.ident)));
                    scopes.set_value(&self.ident, VarValue::Value(global_value));
                }
                None => {
                    let zero_init_value = program.new_value().zero_init(Type::get_i32());
                    let global_value = program.new_value().global_alloc(zero_init_value);
                    program.set_value_name(global_value, Some(format!("@{}", self.ident)));
                    scopes.set_value(&self.ident, VarValue::Value(global_value));
                }
            }
            return;
        }
        // 生成一个alloc指令
        let value = scopes.get_current_func_mut().unwrap().new_alloc_entry(
            program,
            Type::get_i32(),
            Some(&self.ident),
        );
        println!("VarDef: {}", self.ident);
        println!("Value: {:?}", value);
        match self.init_val {
            Some(ref init_val) => {
                let init_value = init_val.generate_program(program, scopes).unwrap();
                let store_inst = scopes
                    .get_current_func()
                    .unwrap()
                    .new_value(program)
                    .store(init_value, value);
                scopes
                    .get_current_func_mut()
                    .unwrap()
                    .push_inst(program, store_inst);
                println!("Init Value: {:?}", init_value);
                println!("Store Inst: {:?}", store_inst);
            }
            None => {}
        }
        scopes.set_value(&self.ident, VarValue::Value(value));
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
        // TODO: Array Type
        // 现阶段所有的参数都是 int 类型的
        let mut params_ty: Vec<Type> = Vec::new();
        if let Some(params) = &self.params {
            params.get_type(&mut params_ty);
        }

        let mut func_data = FunctionData::new(format!("@{}", self.ident), params_ty, func_ty);

        // 接下来, 生成函数的入口块
        let entry_block = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let exit_block = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%exit".to_string()));
        let cur_block = func_data.dfg_mut().new_bb().basic_block(None);
        let mut ret: Option<Value> = None;
        if self.ty == FuncType::Int {
            // 直接把所有返回值扔到一个变量里面
            let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
            func_data
                .dfg_mut()
                .set_value_name(alloc, Some("%retval".to_string()));
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
        // TODO: Arrays
        if let Some(params) = &self.params {
            let program_params = program.func(func).params().to_vec();
            params.generate_program(program, scopes, program_params, &mut function_info);
        }
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
    pub fn generate_program<'a>(
        &'a self,
        _program: &mut Program,
        _scopes: &mut Scopes<'a>,
    ) -> Type {
        match self {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        }
    }
}

impl FuncFParams {
    pub fn generate_program<'a>(
        &'a self,
        program: &mut Program,
        scopes: &mut Scopes<'a>,
        program_params: Vec<Value>,
        func: &mut FunctionInfo,
    ) {
        for (param, program_param) in self.params.iter().zip(program_params) {
            param.generate_program(program, scopes, program_param, func);
        }
    }

    pub fn get_type(&self, params_ty: &mut Vec<Type>) {
        for _param in &self.params {
            params_ty.push(Type::get_i32());
        }
    }
}

impl FuncFParam {
    pub fn generate_program<'a>(
        &'a self,
        program: &mut Program,
        scopes: &mut Scopes<'a>,
        program_param: Value,
        func: &mut FunctionInfo,
    ) {
        // 现阶段所有的参数都是 int 类型的
        let alloc_value = func.new_alloc_entry(program, Type::get_i32(), Some(&self.ident));
        // 然后 把 @x store 到 %x 中
        let store_inst = func.new_value(program).store(program_param, alloc_value);
        func.push_inst(program, store_inst);
        scopes.set_value(&self.ident, VarValue::Value(alloc_value));
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
            Stmt::Assign(lval, exp) => {
                assign_generate_program(program, scopes, lval, exp);
            }
            Stmt::Block(block) => {
                block.generate_program(program, scopes);
            }
            Stmt::If(cond, true_br, false_br) => {
                if_generate_program(cond, true_br, false_br, program, scopes);
            }
            Stmt::While(cond, body) => {
                while_generate_program(cond, body, program, scopes);
            }
            Stmt::Break => {
                let exit = {
                    let (_, exit) = scopes.get_current_loop_mut().unwrap();
                    *exit
                };
                let func_info = scopes.get_current_func_mut().unwrap();
                let jump_inst = func_info.new_value(program).jump(exit);
                func_info.push_inst(program, jump_inst);

                let new_block = func_info.new_bb_dfg(program, None);
                func_info.push_block(program, new_block);
            }
            Stmt::Continue => {
                // 和 break 不同的点在于, continue 要跳转到 entry
                let entry = {
                    let (entry, _) = scopes.get_current_loop_mut().unwrap();
                    *entry
                };
                let func_info = scopes.get_current_func_mut().unwrap();
                let jump_inst = func_info.new_value(program).jump(entry);
                func_info.push_inst(program, jump_inst);

                let new_block = func_info.new_bb_dfg(program, None);
                func_info.push_block(program, new_block);
            }
            Stmt::Exp(exp) => match exp {
                Some(exp) => {
                    exp.generate_program(program, scopes);
                }
                None => {}
            },
        }
    }
}

pub fn return_generate_program<'a>(
    ret_val: &Option<Exp>,
    program: &mut Program,
    scopes: &mut Scopes<'a>,
) {
    match ret_val {
        Some(exp) => {
            let value = exp.generate_program(program, scopes).unwrap_int();
            let func_info = scopes.get_current_func_mut().unwrap();
            let retval = func_info.get_return_value().unwrap();
            let ret_inst = func_info.new_value(program).store(value, retval);
            func_info.push_inst(program, ret_inst);
        }
        None => {
            // 这是一个没有返回值的函数
            // 直接写一个 ret 就可以了
            let func_info = scopes.get_current_func_mut().unwrap();
            let ret_inst = func_info.new_value(program).ret(None);
            func_info.push_inst(program, ret_inst);
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

pub fn assign_generate_program(program: &mut Program, scopes: &mut Scopes, lval: &LVal, exp: &Exp) {
    let lval_value = lval.generate_program(program, scopes).unwrap_int();
    let exp_value = exp.generate_program(program, scopes).unwrap_int();
    let func_info = scopes.get_current_func_mut().unwrap();
    let store_inst = func_info.new_value(program).store(exp_value, lval_value);
    func_info.push_inst(program, store_inst);
}

pub fn if_generate_program<'a>(
    cond: &Exp,
    true_br: &'a Box<Stmt>,
    false_br: &'a Option<Box<Stmt>>,
    program: &mut Program,
    scopes: &mut Scopes<'a>,
) {
    let cond_value = cond.generate_program(program, scopes).unwrap_int();
    let func_info = scopes.get_current_func_mut().unwrap();
    let true_block = func_info.new_bb_dfg(program, Some("%if_true"));
    let false_block = func_info.new_bb_dfg(program, Some("%if_false"));
    let branch_inst = func_info
        .new_value(program)
        .branch(cond_value, true_block, false_block);
    func_info.push_inst(program, branch_inst);

    // 处理 true_block
    func_info.push_block(program, true_block);
    true_br.generate_program(program, scopes);
    let func_info = scopes.get_current_func_mut().unwrap();
    let end_block = func_info.new_bb_dfg(program, Some("%if_end"));
    let jump_inst = func_info.new_value(program).jump(end_block);
    func_info.push_inst(program, jump_inst);

    // 处理 false_block
    func_info.push_block(program, false_block);
    if let Some(false_branch) = false_br {
        false_branch.generate_program(program, scopes);
    }
    let func_info = scopes.get_current_func_mut().unwrap();
    let jump_inst = func_info.new_value(program).jump(end_block);
    func_info.push_inst(program, jump_inst);

    // 产生 end_block
    func_info.push_block(program, end_block);
}

pub fn while_generate_program<'a>(
    cond: &Exp,
    body: &'a Stmt,
    program: &mut Program,
    scopes: &mut Scopes<'a>,
) {
    let func_info = scopes.get_current_func_mut().unwrap();
    // 首先进入 entry block
    let entry_block = func_info.new_bb_dfg(program, Some("%while_entry"));
    let jump_inst = func_info.new_value(program).jump(entry_block);
    func_info.push_inst(program, jump_inst);
    func_info.push_block(program, entry_block);
    let cond_value = cond.generate_program(program, scopes).unwrap_int();

    // 接下来处理 entry 的跳转语句
    let func_info = scopes.get_current_func_mut().unwrap();
    let body_block = func_info.new_bb_dfg(program, Some("%while_body"));
    let exit_block = func_info.new_bb_dfg(program, Some("%while_exit"));
    let branch_inst = func_info
        .new_value(program)
        .branch(cond_value, body_block, exit_block);
    func_info.push_inst(program, branch_inst);

    // 处理 body_block
    func_info.push_block(program, body_block);
    scopes.enter_loop(entry_block, exit_block);
    body.generate_program(program, scopes);
    scopes.exit_loop();
    let func_info = scopes.get_current_func_mut().unwrap();
    let jump_inst = func_info.new_value(program).jump(entry_block);
    func_info.push_inst(program, jump_inst);

    // 处理 exit_block
    func_info.push_block(program, exit_block);
}

impl Exp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        self.lor_exp.generate_program(program, scopes)
    }
}

impl LOrExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            LOrExp::And(and_exp) => and_exp.generate_program(program, scopes).clone(),
            LOrExp::Or(left, _, right) => {
                // 直接实现短路求值
                let result = scopes
                    .get_current_func()
                    .unwrap()
                    .new_value(program)
                    .alloc(Type::get_i32());
                scopes
                    .get_current_func_mut()
                    .unwrap()
                    .push_inst(program, result);
                let left_value = left.generate_program(program, scopes).unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let zero = func_info.new_value(program).integer(0);
                // 要把 left_value 和 zero 比较
                let lhs_inst =
                    func_info
                        .new_value(program)
                        .binary(BinaryOp::NotEq, left_value, zero);
                func_info.push_inst(program, lhs_inst);
                // 把这个结果存到 result 中
                let store_inst = func_info.new_value(program).store(lhs_inst, result);
                func_info.push_inst(program, store_inst);
                // 现在开始短路求值
                // 有两个目的地, 一个是 right, 一个是 exit_block
                let right_block = func_info.new_bb_dfg(program, Some("%or_false"));
                let exit_block = func_info.new_bb_dfg(program, Some("%or_end"));
                // 如果 left_value == 0, 那么跳转到 right_block
                // 否则跳转到 exit_block
                let branch_inst =
                    func_info
                        .new_value(program)
                        .branch(lhs_inst, exit_block, right_block);
                func_info.push_inst(program, branch_inst);
                // 先处理 right_block
                func_info.push_block(program, right_block);
                let right_value = right.generate_program(program, scopes).unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let rhs_inst =
                    func_info
                        .new_value(program)
                        .binary(BinaryOp::NotEq, right_value, zero);
                func_info.push_inst(program, rhs_inst);
                // 把这个结果存到 result 中
                let store_inst = func_info.new_value(program).store(rhs_inst, result);
                func_info.push_inst(program, store_inst);
                // 跳转到 exit_block
                let jump_inst = func_info.new_value(program).jump(exit_block);
                func_info.push_inst(program, jump_inst);
                // 处理 exit_block
                func_info.push_block(program, exit_block);
                let load_inst = func_info.new_value(program).load(result);
                func_info.push_inst(program, load_inst);
                ExpResult::Int(load_inst)
            }
        }
    }
}

impl LAndExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.generate_program(program, scopes),
            LAndExp::And(left, _, right) => {
                // 直接实现短路求值
                let result = scopes
                    .get_current_func()
                    .unwrap()
                    .new_value(program)
                    .alloc(Type::get_i32());
                scopes
                    .get_current_func_mut()
                    .unwrap()
                    .push_inst(program, result);
                let left_value = left.generate_program(program, scopes).unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let zero = func_info.new_value(program).integer(0);
                // 要把 left_value 和 zero 比较
                let lhs_inst =
                    func_info
                        .new_value(program)
                        .binary(BinaryOp::NotEq, left_value, zero);
                func_info.push_inst(program, lhs_inst);
                // 把这个结果存到 result 中
                let store_inst = func_info.new_value(program).store(lhs_inst, result);
                func_info.push_inst(program, store_inst);
                // 现在开始短路求值
                // 有两个目的地, 一个是 right, 一个是 exit_block
                let right_block = func_info.new_bb_dfg(program, Some("%and_true"));
                let exit_block = func_info.new_bb_dfg(program, Some("%and_end"));
                // 如果 left_value == 0, 那么跳转到 exit_block
                // 否则跳转到 right_block
                let branch_inst =
                    func_info
                        .new_value(program)
                        .branch(lhs_inst, right_block, exit_block);
                func_info.push_inst(program, branch_inst);
                // 先处理 right_block
                func_info.push_block(program, right_block);
                let right_value = right.generate_program(program, scopes).unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let rhs_inst =
                    func_info
                        .new_value(program)
                        .binary(BinaryOp::NotEq, right_value, zero);
                func_info.push_inst(program, rhs_inst);
                // 把这个结果存到 result 中
                let store_inst = func_info.new_value(program).store(rhs_inst, result);
                func_info.push_inst(program, store_inst);
                // 跳转到 exit_block
                let jump_inst = func_info.new_value(program).jump(exit_block);
                func_info.push_inst(program, jump_inst);
                // 处理 exit_block
                func_info.push_block(program, exit_block);
                let load_inst = func_info.new_value(program).load(result);
                func_info.push_inst(program, load_inst);
                ExpResult::Int(load_inst)
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
                let binary_op = match op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::Ne => BinaryOp::NotEq,
                };
                let lhs_value = left_value.unwrap_int();
                let rhs_value = right_value.unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let binary_inst = func_info
                    .new_value(program)
                    .binary(binary_op, lhs_value, rhs_value);
                func_info.push_inst(program, binary_inst);
                ExpResult::Int(binary_inst)
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
                let binary_op = match op {
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Le => BinaryOp::Le,
                    RelOp::Ge => BinaryOp::Ge,
                };
                let lhs_value = left_value.unwrap_int();
                let rhs_value = right_value.unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let binary_inst = func_info
                    .new_value(program)
                    .binary(binary_op, lhs_value, rhs_value);
                func_info.push_inst(program, binary_inst);
                ExpResult::Int(binary_inst)
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
                let binary_op = match op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };
                let lhs_value = left_value.unwrap_int();
                let rhs_value = right_value.unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let binary_inst = func_info
                    .new_value(program)
                    .binary(binary_op, lhs_value, rhs_value);
                func_info.push_inst(program, binary_inst);
                ExpResult::Int(binary_inst)
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
                let binary_op = match op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                };
                let lhs_value = left_value.unwrap_int();
                let rhs_value = right_value.unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                let binary_inst = func_info
                    .new_value(program)
                    .binary(binary_op, lhs_value, rhs_value);
                func_info.push_inst(program, binary_inst);
                ExpResult::Int(binary_inst)
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
                let int_value = value.unwrap_int();
                let func_info = scopes.get_current_func_mut().unwrap();
                match op {
                    UnaryOp::Pos => value,
                    UnaryOp::Neg => {
                        let zero = func_info.new_value(program).integer(0);
                        let sub_inst =
                            func_info
                                .new_value(program)
                                .binary(BinaryOp::Sub, zero, int_value);
                        func_info.push_inst(program, sub_inst);
                        ExpResult::Int(sub_inst)
                    }
                    UnaryOp::Not => {
                        let zero = func_info.new_value(program).integer(0);
                        let not_inst =
                            func_info
                                .new_value(program)
                                .binary(BinaryOp::Eq, int_value, zero);
                        func_info.push_inst(program, not_inst);
                        ExpResult::Int(not_inst)
                    }
                }
            }
            UnaryExp::Call(ident, args) => {
                let mut params = Vec::new();
                if let Some(args) = args {
                    for arg in &args.params {
                        params.push(arg.generate_program(program, scopes).unwrap_int());
                    }
                }
                println!("Call: {}", ident);
                let callee = *scopes.get_func(ident).unwrap();
                let func_info = scopes.get_current_func_mut().unwrap();
                let call_inst = func_info.new_value(program).call(callee, params);
                func_info.push_inst(program, call_inst);
                ExpResult::Int(call_inst)
            }
        }
    }
}

impl PrimaryExp {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        match self {
            PrimaryExp::LVal(lval) => {
                // Load the alloc value
                let value = lval.generate_program(program, scopes).unwrap_int();
                if scopes.is_global_scope() {
                    return ExpResult::Int(value);
                }
                
                
                let ty: &ValueKind; 
                if value.is_global() {
                    let func_info = scopes.get_current_func_mut().unwrap();
                    let load_inst = func_info
                        .new_value(program)
                        .load(value);
                    func_info.push_inst(program, load_inst);
                    return ExpResult::Int(load_inst);
                } else {
                    ty = program
                        .func(*scopes.get_current_func().unwrap().get_func())
                        .dfg()
                        .value(value)
                        .kind();
                };
                match ty {
                    ValueKind::Integer(_) => ExpResult::Int(value),
                    _ => {
                        let func_info = scopes.get_current_func_mut().unwrap();
                        let load_inst = func_info.new_value(program).load(value);
                        func_info.push_inst(program, load_inst);
                        ExpResult::Int(load_inst)
                    }
                }
            }

            PrimaryExp::Number(x) => ExpResult::Int(if scopes.is_global_scope() {
                program.new_value().integer(*x)
            } else {
                scopes
                    .get_current_func_mut()
                    .unwrap()
                    .new_value(program)
                    .integer(*x)
            }),
            PrimaryExp::Exp(exp) => exp.generate_program(program, scopes),
        }
    }
}

impl LVal {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> ExpResult {
        let var_value = scopes.get_value(&self.ident).cloned();
        match var_value {
            Some(VarValue::Const(x)) => {
                if scopes.is_global_scope() {
                    return ExpResult::Int(program.new_value().integer(x));
                }
                let func_info = scopes.get_current_func_mut().unwrap();
                ExpResult::Int(func_info.new_value(program).integer(x))
            }
            Some(VarValue::Value(value)) => {
                println!("I have arrived here. {}", self.ident);
                // 我得到的是一个 alloc
                ExpResult::Int(value)
            }
            _ => ExpResult::Void,
        }
    }
}

impl InitVal {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes) -> Option<Value> {
        match self {
            InitVal::Exp(exp) => Some(exp.generate_program(program, scopes).unwrap_int()),
            InitVal::Array(_array) => {
                // 暂时先不处理数组
                None
            }
        }
    }
}
