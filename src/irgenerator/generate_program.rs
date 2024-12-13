//! 生成内存形式的IR代码

use koopa::ir::{
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type, TypeKind, Value,
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
                "@getarray".to_string(),
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
        // 现阶段都是int类型的常量 + int 类型的数组等.
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
        // 在 init_val 生成程序的过程中就进行填充
        // 要先通过 array_index 确定这个常量的类型
        let ty = get_type(&self.array_index, scopes);
        let init_val = self.init_val.generate_program(program, scopes);
        let reshaped = init_val.reshape(&mut ty.clone());

        if ty.is_i32() {
            // 当前的 ConstDef 是一个 int 类型的常量
            match reshaped {
                InitializedValue::Const(int_num) => {
                    scopes.set_value(&self.ident, VarValue::Const(int_num));
                }
                _ => unreachable!(),
            }
        } else {
            // 是一个数组
            if scopes.is_global_scope() {
                let const_val = reshaped.to_const(program, scopes); // 给每一个元素都常量求值
                let global_value = program.new_value().global_alloc(const_val);
                program.set_value_name(global_value, Some(format!("@{}", self.ident)));
                scopes.set_value(&self.ident, VarValue::Value(global_value));
            } else {
                let func_info = scopes.get_current_func_mut().unwrap();
                let alloc_inst = func_info.new_alloc_entry(program, ty, Some(&self.ident));
                reshaped.generate_store_inst(program, scopes, alloc_inst);
                scopes.set_value(&self.ident, VarValue::Value(alloc_inst));
            }
        }
    }
}

pub fn get_type(array_index: &Vec<ConstExp>, scopes: &mut Scopes) -> Type {
    let mut ty = Type::get_i32();
    for index in array_index.iter().rev() {
        let value = index.evaluate(scopes);
        ty = Type::get_array(ty, value as usize);
    }
    ty
}

pub fn get_temporary_value_int(program: &mut Program, scopes: &Scopes, num: i32) -> Value {
    if scopes.is_global_scope() {
        program.new_value().integer(num)
    } else {
        let func_info = scopes.get_current_func().unwrap();
        func_info.new_value(program).integer(num)
    }
}

pub fn get_temporary_value_array(
    program: &mut Program,
    scopes: &Scopes,
    values: Vec<Value>,
) -> Value {
    if scopes.is_global_scope() {
        program.new_value().aggregate(values)
    } else {
        let func_info = scopes.get_current_func().unwrap();
        func_info.new_value(program).aggregate(values)
    }
}

impl ConstInitVal {
    pub fn generate_program(
        &self,
        program: &mut Program,
        scopes: &mut Scopes,
    ) -> InitializedValue {
        match self {
            ConstInitVal::Exp(exp) => InitializedValue::Const(exp.evaluate(scopes)),
            ConstInitVal::Array(array) => {
                let mut values = Vec::new();
                for init_val in array {
                    let value = init_val.generate_program(program, scopes);
                    values.push(value);
                }
                InitializedValue::Array(values)
            }
        }
    }
}


impl VarDecl {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        for def in &self.defs {
            def.generate_program(program, scopes);
        }
    }
}

impl VarDef {
    pub fn generate_program<'a>(&'a self, program: &mut Program, scopes: &mut Scopes<'a>) {
        // 首先, 这个变量的类型可能只是单纯的int, 也可能是数组
        // 此时应该处理数组的情况
        println!("VarDef: {}", self.ident);
        let ty = get_type(&self.array_index, scopes);
        /*
         * 根据是否是全局变量, 生成不同的代码
         */
        if scopes.is_global_scope() {
            match self.init_val {
                Some(ref init_val) => {
                    // 全局变量的初始化必须也是常量
                    let real_val = init_val.generate_program(program, scopes);
                    let reshaped = real_val.reshape(&mut ty.clone());
                    let value = reshaped.to_const(program, scopes);
                    let global_value = program.new_value().global_alloc(value);
                    program.set_value_name(global_value, Some(format!("@{}", self.ident)));
                    scopes.set_value(&self.ident, VarValue::Value(global_value));
                }
                None => {
                    println!("No init_val");
                    let zero_init_value = program.new_value().zero_init(ty);
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
            ty.clone(),
            Some(&self.ident),
        );
        // 即使是局部变量的数组, 也要补0 + 初始化

        if let Some(init) = &self.init_val {
            let real_val = init.generate_program(program, scopes);
            let reshaped = real_val.reshape(&mut ty.clone());
            reshaped.generate_store_inst(program, scopes, value);
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
        println!("FuncDef: {}", self.ident);
        let func_ty = self.ty.generate_program(program, scopes);
        // 现阶段所有的参数都是 int 类型的
        let mut params_ty: Vec<Type> = Vec::new();
        if let Some(params) = &self.params {
            // 直接把所有的参数类型放到 params_ty 中
            params.get_type(&mut params_ty, scopes);
        }
        let mut func_data =
            FunctionData::new(format!("@{}", self.ident), params_ty.clone(), func_ty);

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
        if let Some(params) = &self.params {
            let formal_params = program.func(func).params().to_vec();
            params.generate_program(
                program,
                scopes,
                &params_ty,
                &mut function_info,
                &formal_params,
            );
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
        params_ty: &Vec<Type>,
        func: &mut FunctionInfo,
        formal_params: &Vec<Value>,
    ) {
        for ((param, param_ty), formal_param) in
            self.params.iter().zip(params_ty).zip(formal_params)
        {
            param.generate_program(program, scopes, func, param_ty.clone(), formal_param);
        }
    }

    pub fn get_type(&self, params_ty: &mut Vec<Type>, scopes: &mut Scopes) {
        for param in &self.params {
            params_ty.push(param.get_type(scopes));
        }
    }
}

impl FuncFParam {
    pub fn generate_program<'a>(
        &'a self,
        program: &mut Program,
        scopes: &mut Scopes<'a>,
        func: &mut FunctionInfo,
        ty: Type,
        formal_param: &Value,
    ) {
        // 现阶段所有的参数都是 int 类型的
        let alloc_value = func.new_alloc_entry(program, ty, Some(&self.ident));
        // 然后 把 %x store 到 @x 中
        let store_inst = func.new_value(program).store(*formal_param, alloc_value);
        func.push_inst(program, store_inst);
        scopes.set_value(&self.ident, VarValue::Value(alloc_value));
    }

    pub fn get_type(&self, scopes: &mut Scopes) -> Type {
        if self.array_index.is_none() {
            Type::get_i32()
        } else {
            // 如果是一个Some, 但是是个空, 是个 arr[]
            let ty = if self.array_index.as_ref().unwrap().is_empty() {
                Type::get_pointer(Type::get_i32())
            } else {
                let base = get_type(&self.array_index.as_ref().unwrap(), scopes);
                Type::get_pointer(base)
            };
            ty
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
            let value = exp.generate_program(program, scopes).unwrap_int(program, scopes);
            let func_info = scopes.get_current_func_mut().unwrap();
            let retval = func_info.get_return_value().unwrap();
            let ret_inst = func_info.new_value(program).store(value, retval);
            func_info.push_inst(program, ret_inst);
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

pub fn assign_generate_program(program: &mut Program, scopes: &mut Scopes, lval: &LVal, exp: &Exp) {
    println!("Assgn {} {:?} = {:?}", lval.ident, lval.array_index, exp);
    let lval_value = lval.generate_program(program, scopes).unwrap_int_ptr();
    let lkind = scopes.get_var_typekind(&lval_value, program);
    println!("lkind: {}", lkind);
    let exp_value = exp.generate_program(program, scopes).unwrap_int(program, scopes);
    let rkind = scopes.get_var_typekind(&exp_value, program);
    println!("rkind: {}", rkind);
    let func_info = scopes.get_current_func().unwrap();
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
    let cond_value = cond.generate_program(program, scopes).unwrap_int(program, scopes);
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
    let cond_value = cond.generate_program(program, scopes).unwrap_int(program, scopes);

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
                let left_value = left.generate_program(program, scopes).unwrap_int(program, scopes);
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
                let right_value = right.generate_program(program, scopes).unwrap_int(program, scopes);
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
                let left_value = left.generate_program(program, scopes).unwrap_int(program, scopes);
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
                let right_value = right.generate_program(program, scopes).unwrap_int(program, scopes);
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
                let lhs_value = left_value.unwrap_int(program, scopes);
                let rhs_value = right_value.unwrap_int(program, scopes);
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
                let lhs_value = left_value.unwrap_int(program, scopes);
                let rhs_value = right_value.unwrap_int(program, scopes);
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
                let lhs_value = left_value.unwrap_int(program, scopes);
                let rhs_value = right_value.unwrap_int(program, scopes);
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
                let lhs_value = left_value.unwrap_int(program, scopes);
                let rhs_value = right_value.unwrap_int(program, scopes);
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
                let int_value = value.unwrap_int(program, scopes);
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
                // 传递数组参数相当于传递第一个元素的地址
                if let Some(args) = args {
                    for arg in &args.params {
                        // 把 ArrayPtr 转换为 Value
                        params.push(arg.generate_program(program, scopes).unwrap_val(program, scopes));
                    }
                }
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
            PrimaryExp::LVal(lval) => lval.generate_program(program, scopes),
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
        println!("LVal: {}", self.ident);
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
                // 我得到的是一个 alloc
                // 现在无论如何都是一个Pointer
                // 但是这个Pointer的Base可能是整数, 数组以及一个指针
                // 如果是 arr[2][3], 取arr[1]的话必须是一个 ArrayPtr
                // 但是如果是 arr[1][1] 就应该是一个IntPtr
                let mut is_ptr = false; // 判断扒了皮是不是还是一个Pointer
                // 如果扒了皮还是Pointer就必须先 Load
                let mut dims = 0;
                let kind = scopes.get_var_typekind(&value, program);
                println!("base kind: {}", kind);
                match kind {
                    TypeKind::Pointer(base) => {
                        let mut now_ty = &base;
                        // 现在试图把数组的维数还原出来
                        loop {
                            match now_ty.kind() {
                                TypeKind::Array(base, _len) => {
                                    now_ty = base;
                                }
                                TypeKind::Pointer(base) => {
                                    now_ty = base;
                                    is_ptr = true;
                                }
                                _ => break,
                            }
                            dims += 1;
                        }
                    }
                    _ => unreachable!(),
                }
                println!("The dims is {}", dims);
                println!("The is_ptr is {}", is_ptr);

                let mut now_value = value;

                if is_ptr {
                    let func_info = scopes.get_current_func().unwrap();
                    let load_inst = func_info.new_value(program).load(value);
                    func_info.push_inst(program, load_inst);
                    now_value = load_inst;
                }

                let mut count = 0;
                let mut ptr = now_value;
                // 对于 arr[][5], 读取 arr[1] 就要先 get_ptr
                // 对于第二个开始再进行 get_elem_ptr
                for index in &self.array_index {
                    let index_value = index.generate_program(program, scopes).unwrap_int(program, scopes);
                    let func_info = scopes.get_current_func().unwrap();
                    
                    println!("entry_ty: {}", scopes.get_var_typekind(&ptr, program));

                    ptr = if is_ptr && count == 0 {
                        println!("The FIRST ptr is {:?}", ptr);
                        let ptr_inst = func_info.new_value(program).get_ptr(ptr, index_value);
                        func_info.push_inst(program, ptr_inst);
                        ptr_inst
                    } else {
                        let elem_ptr_inst = func_info.new_value(program).get_elem_ptr(ptr, index_value);
                        func_info.push_inst(program, elem_ptr_inst);
                        elem_ptr_inst
                    };
                    count += 1;

                    println!("end_ty: {}", scopes.get_var_typekind(&ptr, program));

                }



                if count == dims {
                    // 是一个一直 dereference 到 Int 的指针
                    println!("The Int ptr is {:?}", ptr);
                    ExpResult::IntPtr(ptr)
                } else {
                    // 是一个类似于数组参数一样的东西
                    // 传递数组参数相当于传递其第一个元素的地址
                    // arr, arr[1] 都是数组参数
                    // 第一维省略了所以要用 get_ptr
                    // 但是后面没省略的要用回 get_elem_ptr
                    /*
                     * %17 = load @arr
                     * %18 = load @i
                     * %19 = getptr %17, %18
                     * %20 = getelemptr %19, 0
                     */ 
                    if !is_ptr || !self.array_index.is_empty() {
                        let func_info = scopes.get_current_func().unwrap();
                        let zero = func_info.new_value(program).integer(0);
                        let load_inst = func_info.new_value(program).get_elem_ptr(ptr, zero);
                        func_info.push_inst(program, load_inst);
                        ExpResult::ArrayPtr(load_inst)
                    } else {
                        ExpResult::ArrayPtr(ptr)
                    }
                }

            }
            _ => ExpResult::Void,
        }
    }
}

#[derive(Debug, Clone)]
pub enum InitializedValue {
    Const(i32),
    ExpValue(Value),
    Array(Vec<InitializedValue>),
}

impl InitVal {
    pub fn generate_program(
        &self,
        program: &mut Program,
        scopes: &mut Scopes,
    ) -> InitializedValue {
        match self {
            InitVal::Exp(exp) => {
                if scopes.is_global_scope() {
                    return InitializedValue::Const(exp.evaluate(scopes));
                }
                InitializedValue::ExpValue(exp.generate_program(program, scopes).unwrap_int(program, scopes))
            }
            InitVal::Array(array) => {
                let mut values = Vec::new();
                for val in array {
                    values.push(val.generate_program(program, scopes));
                }
                InitializedValue::Array(values)
            }
        }
    }
}


impl InitializedValue {

    /// 重塑一个数组, 例如: [2][3][4] = [(4, 4), (3, 12), (2, 24)]
    pub fn reshape(self, ty: &Type) -> Self {
        let mut lens = Vec::new();
        // 得到了一个reshape后的数组长度
        let mut ty = ty;
        loop {
            match ty.kind() {
                TypeKind::Int32 => break,
                TypeKind::Array(base, len) => {
                    lens.push(*len);
                    ty = base;
                }
                _ => unreachable!()
            }
        }

        let mut new_lens = Vec::new();
        let mut last_len = 1;
        for len in lens.iter().rev() {
            last_len = last_len * len;
            new_lens.push((*len, last_len));
        }

        match self {
            InitializedValue::Const(_) => self,
            InitializedValue::ExpValue(_) => self,
            InitializedValue::Array(a) => {
                // lens不能为空
                Self::array_reshape(a, &new_lens)
            }
        }
    }

    pub fn array_reshape(inits: Vec<InitializedValue>, lens: &Vec<(usize, usize)>) -> Self {
        let mut now_array: Vec<Vec<Self>> = vec![vec![]; lens.len() + 1];
        // now_array[idx] 表示第 idx 维当前放置的元素
        // arr[2][3][4] = {1, 2, 3, 4}
        // now_array[0] 里面就会先放入 {1, 2, 3, 4}
        let mut count = 0;

        for init in inits {
            match init {
                Self::Array(a) => {
                    // 现在是未能完全对齐的情况
                    // 找到当前第一个不空的维度
                    let now_dim = match now_array.iter().position(|a| !a.is_empty()) {
                        Some(0) => unreachable!(),
                        Some(x) => &lens[..x],
                        None => &lens[..lens.len() - 1],
                    };
                    // 现在向当前维度放入塑形后的初始化列表
                    now_array[now_dim.len()].push(Self::array_reshape(a, &now_dim.to_vec()));
                    Self::packen(&mut now_array, lens);
                    count += now_dim.last().unwrap().1; // 已经放入的元素个数
                }
                _ => {
                    // 剩下的是 Const 或者 ExpValue
                    now_array[0].push(init);
                    Self::packen(&mut now_array, lens);
                    count += 1;
                }
            }
        }

        while count < lens.last().unwrap().1 {
            // 没能满足整个数组
            now_array[0].push(Self::Const(0));
            Self::packen(&mut now_array, lens);
            count += 1;
        }
        // 之所以使用pop是因为它传的不是引用
        now_array.pop().unwrap().pop().unwrap()
    }

    pub fn packen(now_array: &mut Vec<Vec<Self>>, lens: &Vec<(usize, usize)>) {
        // 现在要把 now_array 里面的元素打包到上一层
        let mut idx = 0;
        while idx < lens.len() {
            let mut new_array = Vec::new();
            let (len, _total) = lens[idx];
            if now_array[idx].len() == len {
                // 找到了一个已经填满的维度
                new_array.append(&mut now_array[idx]);
                now_array[idx].clear();
                now_array[idx + 1].push(Self::Array(new_array));
            }
            idx += 1;
        }
    }

    pub fn generate_store_inst(&self, program: &mut Program, scopes: &Scopes, alloc_inst: Value) {
        match self {
            InitializedValue::Const(num) => {
                let value = get_temporary_value_int(program, scopes, *num);
                let func_info = scopes.get_current_func().unwrap();
                let store_inst = func_info.new_value(program).store(value, alloc_inst);
                func_info.push_inst(program, store_inst);
            }
            InitializedValue::ExpValue(value) => {
                let func_info = scopes.get_current_func().unwrap();
                let store_inst = func_info.new_value(program).store(*value, alloc_inst);
                func_info.push_inst(program, store_inst);
            }
            InitializedValue::Array(values) => {
                for (index, value) in values.iter().enumerate() {
                    let index_value = get_temporary_value_int(program, scopes, index as i32);
                    let func_info = scopes.get_current_func().unwrap();
                    let pointer_inst = 
                        func_info
                            .new_value(program)
                            .get_elem_ptr(alloc_inst, index_value);
                    func_info.push_inst(program, pointer_inst);
                    value.generate_store_inst(program, scopes, pointer_inst);
                }
            }
        }
    }


    pub fn to_const(&self, program: &mut Program, scopes: &Scopes) -> Value {
        match self {
            InitializedValue::Const(num) => get_temporary_value_int(program, scopes, *num),
            InitializedValue::ExpValue(_) => unreachable!(), // 常量不了
            InitializedValue::Array(values) => {
                let mut new_values = Vec::new();
                for value in values {
                    new_values.push(value.to_const(program, scopes));
                }
                let array_value = get_temporary_value_array(program, scopes, new_values);
                array_value
            }
        }
    }
}
