//! 生成内存形式的IR代码

use koopa::ir::{
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind,
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
        let init_val = self.init_val.generate_program(program, scopes, &ty);

        if ty.is_i32() {
            // 当前的 ConstDef 是一个 int 类型的常量
            match init_val {
                VarValue::Const(int_num) => {
                    scopes.set_value(&self.ident, VarValue::Const(int_num));
                }
                _ => unreachable!(),
            }
        } else {
            // 是一个数组
            let real_val = match init_val {
                VarValue::Value(value) => value,
                _ => unreachable!(),
            };
            if scopes.is_global_scope() {
                let global_value = program.new_value().global_alloc(real_val);
                program.set_value_name(global_value, Some(format!("@{}", self.ident)));
                scopes.set_value(&self.ident, VarValue::Value(global_value));
            } else {
                let func_info = scopes.get_current_func_mut().unwrap();
                let alloc_inst = func_info.new_alloc_entry(program, ty, Some(&self.ident));
                generate_store_inst(program, scopes, &self.ident, real_val, alloc_inst);
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


/// 生成 store 指令
pub fn generate_store_inst(
    program: &mut Program,
    scopes: &Scopes,
    ident: &str,
    real_val: Value,
    alloc_inst: Value,
) {
    // 其实不一定哪里都要加mut的
    let func_info = scopes.get_current_func().unwrap();
    let value_kind = func_info.get_local_valuekind(program, real_val);
    match value_kind {
        ValueKind::Integer(_) => {
            let store_inst = func_info.new_value(program).store(real_val, alloc_inst);
            func_info.push_inst(program, store_inst);
        }
        ValueKind::Aggregate(array) => {
            for (index, value) in array.elems().iter().enumerate() {
                let index_value = func_info.new_value(program).integer(index as i32);
                let pointer_inst = func_info
                    .new_value(program)
                    .get_elem_ptr(alloc_inst, index_value);
                func_info.push_inst(program, pointer_inst);
                generate_store_inst(program, scopes, ident, *value, pointer_inst);
            }
        }
        _ => unreachable!(),
    }
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
        ty: &Type,
    ) -> VarValue {
        match self {
            ConstInitVal::Exp(exp) => VarValue::Const(exp.evaluate(scopes)),
            ConstInitVal::Array(array) => {
                let mut values = Vec::new();
                // 把所有的值都填充进去
                let (base, len) = match ty.kind() {
                    TypeKind::Array(base, len) => (base, len),
                    _ => unreachable!(),
                };
                let mut count: usize = 0;
                for val in array {
                    let var_value = val.generate_program(program, scopes, base);
                    match var_value {
                        VarValue::Const(int_num) => {
                            values.push(get_temporary_value_int(program, scopes, int_num));
                        }
                        VarValue::Value(value) => {
                            values.push(value);
                        }
                    }
                    count = count + 1;
                }
                // 如果没有填满, 那么就要填充0
                for _ in count..*len {
                    values.push(get_zero(program, scopes, base));
                }
                let array_value = get_temporary_value_array(program, scopes, values);
                VarValue::Value(array_value)
            }
        }
    }
}

/// 返回一个当前类型的0值
pub fn get_zero(program: &mut Program, scopes: &Scopes, ty: &Type) -> Value {
    match ty.kind() {
        TypeKind::Int32 => get_temporary_value_int(program, scopes, 0),
        TypeKind::Array(base, len) => {
            let mut values = Vec::new();
            for _ in 0..*len {
                values.push(get_zero(program, scopes, base));
            }
            get_temporary_value_array(program, scopes, values)
        }
        _ => unreachable!(),
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
        let reshaped = reshape_ty(&ty);
        /*
         * 根据是否是全局变量, 生成不同的代码
         */
        if scopes.is_global_scope() {
            match self.init_val {
                Some(ref init_val) => {
                    // 全局变量的初始化必须也是常量
                    let real_val = if ty.is_i32() {
                        let value = init_val.evaluate(scopes);
                        let val = program.new_value().integer(value);
                        val
                    } else {
                        // 数组
                        let value = init_val.to_const(program, scopes, &ty, &reshaped);
                        let val = match value {
                            VarValue::Const(int_num) => {
                                // 其实没有必要
                                let val = program.new_value().integer(int_num);
                                val
                            }
                            VarValue::Value(value) => value,
                        };
                        val
                    };
                    let global_value = program.new_value().global_alloc(real_val);
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
        let value =
            scopes
                .get_current_func_mut()
                .unwrap()
                .new_alloc_entry(program, ty.clone(), Some(&self.ident));
        // 即使是局部变量的数组, 也要补0 + 初始化
        
        let real_val = match self.init_val {
            Some(ref init_val) => {
                // 加入数组
                let init_value = init_val.generate_program(program, scopes, &ty);
                init_value
            }
            None => {
                InitializedValue::get_zero(program, scopes, &ty)
            }
        };

        real_val.generate_store_inst(program, scopes, value);
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
        // 现阶段所有的参数都是 int 类型的
        let mut params_ty: Vec<Type> = Vec::new();
        if let Some(params) = &self.params {
            // 直接把所有的参数类型放到 params_ty 中
            params.get_type(&mut params_ty, scopes);
        }
        let mut func_data = FunctionData::new(format!("@{}", self.ident), params_ty.clone(), func_ty);

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
            params.generate_program(program, scopes, &params_ty, &mut function_info, &formal_params);
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
        for ((param, param_ty), formal_param) in self.params.iter().zip(params_ty).zip(formal_params) {
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
        formal_param: &Value
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
            let value = exp.generate_program(program, scopes).unwrap_int();
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
                // 传递数组参数相当于传递第一个元素的地址
                if let Some(args) = args {
                    for arg in &args.params {
                        // 把 ArrayPtr 转换为 Value
                        params.push(arg.generate_program(program, scopes).unwrap_int());
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
            PrimaryExp::LVal(lval) => {
                // Load the alloc value
                let temp = lval.generate_program(program, scopes);
                match temp {
                    ExpResult::ArrayPtr(p) => {
                        return ExpResult::ArrayPtr(p);
                    }
                    _ => {}
                }
                let value = temp.unwrap_int();
                if scopes.is_global_scope() {
                    return ExpResult::Int(value);
                }

                let ty: &ValueKind;
                if value.is_global() {
                    let func_info = scopes.get_current_func_mut().unwrap();
                    let load_inst = func_info.new_value(program).load(value);
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
                // 我得到的是一个 alloc
                if !self.array_index.is_empty() {
                    // 要用数组, 先要load 进来
                    // %ptr = load @arr
                    let load_inst = scopes
                        .get_current_func_mut()
                        .unwrap()
                        .new_value(program)
                        .load(value);
                    let func_info = scopes.get_current_func().unwrap();
                    func_info.push_inst(program, load_inst);
                    let mut pointer_inst = load_inst;
                    for index in &self.array_index {
                        let index_value = index.generate_program(program, scopes).unwrap_int();
                        let base_ty = program
                            .func(*scopes.get_current_func().unwrap().get_func())
                            .dfg().value(value).ty();
                        pointer_inst = match base_ty.kind() {
                            TypeKind::Array(_, _) => {
                                scopes
                                    .get_current_func()
                                    .unwrap()
                                    .new_value(program)
                                    .get_elem_ptr(load_inst, index_value)
                            },
                            TypeKind::Pointer(_) => {
                                scopes
                                    .get_current_func()
                                    .unwrap()
                                    .new_value(program)
                                    .get_ptr(load_inst, index_value)
                            },
                            _ => unreachable!(),
                        };
                        let func_info = scopes.get_current_func_mut().unwrap();
                        func_info.push_inst(program, pointer_inst);
                    }
                    return ExpResult::Int(pointer_inst)
                }
                // 剩下的没有数组下标, 可能是一个普通的变量
                // 也可能是一个指针
                let ty = program
                    .func(*scopes.get_current_func().unwrap().get_func())
                    .dfg()
                    .value(value)
                    .ty();
                
                let ret = match ty.kind() {
                    TypeKind::Pointer(base) => {
                        if base.is_i32() {
                            return ExpResult::Int(value);
                        }
                        let func_info = scopes.get_current_func().unwrap();
                        let temp_value = get_temporary_value_int(program, scopes, 0);
                        let pointer = func_info.new_value(program).get_elem_ptr(value, temp_value);
                        func_info.push_inst(program, pointer);
                        ExpResult::ArrayPtr(pointer)
                    }
                    TypeKind::Array(_, _) => {
                        let func_info = scopes.get_current_func().unwrap();
                        let temp_value = get_temporary_value_int(program, scopes, 0);
                        let pointer = func_info.new_value(program).get_elem_ptr(value, temp_value);
                        func_info.push_inst(program, pointer);
                        ExpResult::ArrayPtr(pointer)
                    }
                    _ => ExpResult::Int(value),
                };
                ret
            }
            _ => ExpResult::Void,
        }
    }
}

pub enum InitializedValue {
    ExpValue(Value),
    Array(Vec<InitializedValue>),
}

impl InitVal {
    pub fn generate_program(&self, program: &mut Program, scopes: &mut Scopes, ty: &Type) -> InitializedValue {
        match self {
            InitVal::Exp(exp) => InitializedValue::ExpValue(exp.generate_program(program, scopes).unwrap_int()),
            InitVal::Array(array) => {
                let mut values = Vec::new();
                let (base, len) = match ty.kind() {
                    TypeKind::Array(base, len) => (base, len),
                    _ => unreachable!(),
                };
                let mut count = 0;
                for val in array {
                    values.push(val.generate_program(program, scopes, base));
                    count = count + 1;
                }
                for _ in count..*len {
                    values.push(InitializedValue::get_zero(program, scopes, base));
                }
                let array_value = InitializedValue::Array(values);
                array_value
            }
        }
    }

    /// 计算全局变量的初始化
    pub fn to_const(&self, program: &mut Program, scopes: &Scopes, ty: &Type, reshaped: &ReshapeTy) -> VarValue {
        match self {
            InitVal::Exp(exp) => VarValue::Const(exp.evaluate(scopes)),
            InitVal::Array(array) => {
                let mut values = Vec::new();
                // 把所有的值都填充进去
                let (base, len) = match ty.kind() {
                    TypeKind::Array(base, len) => (base, len),
                    _ => unreachable!(),
                };

                
                let mut array_count: usize = 0;
                let mut exp_count: usize = 0;
                for val in array {
                    let var_value = val.to_const(program, scopes, base, reshaped);
                    match var_value {
                        VarValue::Const(int_num) => {
                            values.push(get_temporary_value_int(program, scopes, int_num));
                        }
                        VarValue::Value(value) => {
                            values.push(value);
                        }
                    }
                    array_count += 1;
                }
                // 如果没有填满, 那么就要填充0
                for _ in array_count..*len {
                    values.push(get_zero(program, scopes, base));
                }
                let array_value = get_temporary_value_array(program, scopes, values);
                VarValue::Value(array_value)
            }
        }
    }
}

/// 把一个数组的类型变换为: [2][3][4] = [(4, 4), (3, 12), (2, 24)]

pub struct ReshapeTy {
    pub tys: Vec<(i32, i32)>,
}

impl ReshapeTy {
    pub fn last(&self) -> &(i32, i32) {
        self.tys.last().unwrap()
    }

    pub fn append(&mut self, other: ReshapeTy) {
        for ty in other.tys {
            self.tys.push(ty);
        }
    }

    pub fn push(&mut self, ty: (i32, i32)) {
        self.tys.push(ty);
    }
}

pub fn reshape_ty(ty: &Type) -> ReshapeTy {
    let mut ret = ReshapeTy { tys: Vec::new() };
    match ty.kind() {
        TypeKind::Array(base, len) => {
            let base_shape = reshape_ty(base);
            let total = base_shape.last().1 * (*len as i32);
            ret.append(base_shape);
            ret.push((*len as i32, total));
        }
        _ => {
            ret.push((0, 1));
        }
    }
    ret
}

impl InitializedValue {
    pub fn get_zero(program: &mut Program, scopes: &Scopes, ty: &Type) -> InitializedValue {
        match ty.kind() {
            TypeKind::Int32 => InitializedValue::ExpValue(
                get_temporary_value_int(program, scopes, 0)
            ),
            TypeKind::Array(base, len) => {
                let mut values = Vec::new();
                for _ in 0..*len {
                    values.push(InitializedValue::get_zero(program, scopes, base));
                }
                InitializedValue::Array(values)
            }
            _ => unreachable!(),
        }
    }

    pub fn generate_store_inst(&self, program: &mut Program, scopes: &Scopes, alloc_inst: Value) {
        match self {
            InitializedValue::ExpValue(value) => {
                let func_info = scopes.get_current_func().unwrap();
                let store_inst = func_info.new_value(program).store(*value, alloc_inst);
                func_info.push_inst(program, store_inst);
            }
            InitializedValue::Array(values) => {
                for (index, value) in values.iter().enumerate() {
                    let index_value = get_temporary_value_int(program, scopes, index as i32);
                    let pointer_inst = scopes
                        .get_current_func()
                        .unwrap()
                        .new_value(program)
                        .get_elem_ptr(alloc_inst, index_value);
                    let func_info = scopes.get_current_func().unwrap();
                    func_info.push_inst(program, pointer_inst);
                    value.generate_store_inst(program, scopes, pointer_inst);
                }
            }
        }
    }
}