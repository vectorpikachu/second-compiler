//! 生成 RISCV 的汇编代码

use koopa::ir::entities::ValueData;
use koopa::ir::values::{Aggregate, Binary, Branch, Call, GetElemPtr, GetPtr, GlobalAlloc, Jump, Load, Return, Store};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind};

use super::function_info::FunctionInfo;
use super::program_info::{self, ProgramInfo};
use std::cmp::max;
use std::io::Write;

use super::function_info::RealValue;

use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};

static GLOBAL_FUNC_ID: AtomicI32 = AtomicI32::new(0);

static GLOBAL_NOW_TYPE: AtomicPtr<Type> = AtomicPtr::new(std::ptr::null_mut());

/// 把所有的局部变量都放在栈上 + 寄存器上

pub trait GenerateAsm<'b, 'a> {
    // 这样就可以返回什么乱写了
    type Out;

    /// 我发现 后期的处理中总是会出现对 program_info 的引用出现两次
    /// 但是其实我并不想让第一次取出来的那个值活得和 program_info 一样长
    /// 所以我要指定一个生命周期
    /// 这里的 'b : 'a
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out;
}

pub trait GenerateAsmWithValue {
    type Out;
    fn generate_asm_with_value(
        &self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
        value: &ValueData,
    ) -> Self::Out;
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Program {
    type Out = ();
    // 生成整个程序的汇编代码
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) {
        // 要先处理全局的, 分配在 Data 中的数据
        for value in self.inst_layout() {
            let value_data = self.borrow_value(*value);
            let name = value_data
                .name()
                .as_ref()
                .unwrap()
                .strip_prefix("@")
                .unwrap();
            program_info.insert_value_name(*value, name.to_string());
            writeln!(buf, "  .data").unwrap();
            writeln!(buf, "  .globl {}", name).unwrap();
            writeln!(buf, "{}:", name).unwrap();
            value_data.generate_asm(buf, program_info);
            writeln!(buf).unwrap();
        }

        // 我们对于当前的每个函数都生成汇编代码
        for &func in self.func_layout() {
            let func_info = FunctionInfo::new(func);
            program_info.set_current_func(func_info);
            self.func(func).generate_asm(buf, program_info);
            if self.func(func).layout().entry_bb().is_none() {
                continue;
            }
            writeln!(buf).unwrap();
        }
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Function {
    type Out = String;
    fn generate_asm(&self, _buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let name = program_info.get_program().func(*self).name().to_string();
        name.strip_prefix("@").unwrap().to_string()
    }
}

/// 生成函数的 prologue
pub fn generate_prologue(
    buf: &mut Vec<u8>,
    program_info: &mut ProgramInfo,
    function_data: &FunctionData,
) {
    writeln!(buf, "  .text").unwrap();
    let func_name = function_data.name().strip_prefix("@").unwrap();
    writeln!(buf, "  .globl {}", func_name).unwrap();
    writeln!(buf, "{}:", func_name).unwrap();
    let func_info = program_info.get_current_func_mut().unwrap();
    // 应该先把 BasicBlock名 加入到函数信息中
    let func_name = function_data.name().strip_prefix("@").unwrap();
    let func_id = GLOBAL_FUNC_ID.fetch_add(1, Ordering::SeqCst);
    for (bb, bb_data) in function_data.dfg().bbs() {
        func_info.set_bb_name(*bb, bb_data.name().clone(), func_name.to_string(), func_id);
    }

    /* 函数的返回地址保存在寄存器 ra 中.
     非叶子函数通常需要在 prologue 中将自己的 ra 寄存器保存到栈帧中.
     在 epilogue 中, 非叶子函数需要先从栈帧中恢复 ra 寄存器,
     之后才能执行 ret 指令.
    */

    // 一个永远不会调用其他函数的函数被称为叶子函数

    // 计算要分配的栈空间.
    // 栈空间 = S + R + A
    // S = 需要为局部变量分配的栈空间
    // R = 为函数的返回地址 ra 分配的栈空间, 如果出现了 call = 4, 否则为 0
    // A = 为传参预留的栈空间, A = max{max len_i -8, 0} * 4
    let mut stack_size = 0;
    let mut max_len = 0;
    let mut call_flag = false;
    let mut alloc_size = 0;

    for (_bb, node) in function_data.layout().bbs() {
        for inst in node.insts().keys() {
            let value = function_data.dfg().value(*inst);
            if !value.ty().is_unit() {
                stack_size += 4;
            }
            match value.kind() {
                ValueKind::Call(call) => {
                    call_flag = true;
                    max_len = max(max_len, call.args().len());
                }
                ValueKind::Alloc(_) => {
                    let size = match value.ty().kind() {
                        TypeKind::Pointer(base) => base.size(),
                        _ => 4,
                    };
                    alloc_size += size;
                }
                _ => {}
            }
        }
    }

    func_info.set_call_flag(call_flag);
    if call_flag {
        stack_size += 4;
    }

    let args_size = max(max_len as i64 - 8, 0) * 4;
    stack_size += args_size as i32;
    func_info.set_current_offset(args_size as i32);

    // 接下来分配所有的 alloc
    for (value, _) in function_data.dfg().values() {
        let value_data = function_data.dfg().value(*value);
        if let ValueKind::Alloc(_alloc) = value_data.kind() {
            println!("我事先分配所有的alloc whose value_data: {:?}", value_data);
            let size = match value_data.ty().kind() {
                TypeKind::Pointer(base) => base.size(),
                _ => 4,
            };
            func_info.set_alloc_offset(*value, value_data.ty().kind().clone());
        }
    }
    println!();

    stack_size += alloc_size as i32;

    // 对齐到 16 字节
    stack_size = (stack_size + 15) / 16 * 16;
    func_info.set_stack_size(stack_size as usize);
    // 在函数入口处, 生成更新栈指针的指令, 将栈指针减去 StackSize. 这个过程叫做函数的 prologue.

    // 应该在计算完栈大小之后就为每个变量分配位置

    if stack_size == 0 {
        return;
    }

    if stack_size <= 2047 {
        writeln!(buf, "  addi sp, sp, -{}", stack_size).unwrap();
    } else {
        writeln!(buf, "  li t0, -{}", stack_size).unwrap();
        writeln!(buf, "  add sp, sp, t0").unwrap();
    }

    // 保存 ra 寄存器
    if call_flag {
        store_to_stack(buf, "ra".to_string(), stack_size - 4);
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for FunctionData {
    type Out = ();
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) {
        /*
         RISC-V 汇编中, 函数符号无需声明即可直接使用.
         关于到底去哪里找这些外部符号, 这件事情由链接器负责.
         除此之外, 调用库函数和调用 SysY 内定义的函数并无区别.
         Koopa IR 中, 函数声明是一种特殊的函数, 它们和函数定义是放在一起的.
         也就是说, 在上一节的基础上,
         你需要在扫描函数时跳过 Koopa IR 中的所有函数声明.
        */
        // 没有进入块, 这就是一个声明了
        if self.layout().entry_bb().is_none() {
            return;
        }

        for (value, value_data) in self.dfg().values() {
            if value_data.kind().is_local_inst() && !value_data.used_by().is_empty() {
                let func_info = program_info.get_current_func_mut().unwrap();
                func_info.get_current_offset(value, value_data);
            }
        }

        generate_prologue(buf, program_info, self);
        // BasicBlock - A handle of Koopa IR basic block.
        // BasicBlockNode - insts: KeyNodeList<Value, InstNode, InstMap>,
        for (bb, node) in self.layout().bbs() {
            let bb_name = bb.generate_asm(buf, program_info);
            // 不能直接写在这里, 会引用可变引用两次
            writeln!(buf, "{}:", bb_name).unwrap();
            for inst in node.insts().keys() {
                // Value 也是一个 Handle, 没有办法直接生成
                let value_data = self.dfg().value(*inst);
                println!("\n我正在处理 : {:?}", value_data);
                value_data.generate_asm(buf, program_info);
                // 计算出来的值, 如果被分配了空间就需要存进去
                // 可能还需要被用到 而且 有值产生
                // 比如 alloc i32 不用存
                if !value_data.ty().is_unit() && !value_data.used_by().is_empty() {
                    let flag = match value_data.kind() {
                        ValueKind::Alloc(_) => false,
                        _ => true,
                    };
                    let func_info = program_info.get_current_func_mut().unwrap();
                    if flag {
                        let offset = func_info.get_current_offset(inst, value_data);
                        match value_data.kind() {
                            ValueKind::Call(_) => {
                                store_to_stack(buf, "a0".to_string(), offset);
                            }
                            _ => {
                                store_to_stack(buf, "t0".to_string(), offset);
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for BasicBlock {
    type Out = String;
    fn generate_asm(&self, _buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let func_info = program_info.get_current_func().unwrap();
        let bb_name = func_info.get_bb_name(*self);
        bb_name
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Value {
    type Out = RealValue;

    /// 生成 Value 的汇编代码
    /// 产出是一个 RealValue, 用来表示这个 Value 的位置
    /// 有些地方需要用到之前的 Value , 这就相当于一个指针指向之前的 Value
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        // writeln!(buf,"  {}:", inst_name).unwrap();
        if self.is_global() {
            // 全局变量放在 .data 段
            let option_data = program_info.get_value_name(*self);
            if option_data.is_none() {
                let borrow_value = program_info.get_program().borrow_value(*self);
                let kind = borrow_value.kind().clone();
                drop(borrow_value);
                match kind {
                    ValueKind::Integer(i) => {
                        return RealValue::Const(i.value());
                    }
                    ValueKind::Aggregate(aggregate) => {
                        aggregate.generate_asm(buf, program_info);
                    }
                    _ => {}
                }
                println!("I am here");
                return RealValue::None;
            } else {
                return RealValue::DataSeg(option_data.unwrap());
            }
        } else {
            let func_info = program_info.get_current_func().unwrap();
            let value_data = {
                let program = program_info.get_program();
                program
                    .func(*func_info.get_func())
                    .dfg()
                    .value(*self)
                    .clone()
            };
            println!("Value_data: {:?}", value_data);
            match value_data.kind() {
                ValueKind::Integer(num) => {
                    return RealValue::Const(num.value());
                }
                ValueKind::FuncArgRef(arg) => {
                    // 当用到了 arg 的时候要判断它在寄存器里
                    // 还是在 栈里
                    let arg_index = arg.index();
                    if arg_index < 8 {
                        return RealValue::Reg(format!("a{}", arg_index));
                    } else {
                        let func_info = program_info.get_current_func_mut().unwrap();
                        let stack_size = func_info.get_stack_size();
                        return RealValue::StackPos(
                            ((arg_index - 8) * 4 + stack_size).try_into().unwrap(),
                        );
                    }
                }
                _ => {
                    println!("在这里");
                    let func_info = program_info.get_current_func_mut().unwrap();
                    let option_alloc = func_info.get_alloc_offset(self);
                    let is_array = func_info.is_alloc_array(self);

                    if option_alloc.is_some() && is_array {
                        println!("I Got a array");
                        return RealValue::Array(option_alloc.unwrap());
                    }

                    let offset = func_info.get_current_offset(self);

                    // 把类型存入, 如果是get_elem_ptr的话
                    if let ValueKind::GetElemPtr(_) = value_data.kind() {
                        let now_ty = GLOBAL_NOW_TYPE.load(Ordering::SeqCst);
                        func_info.insert_type(*self, unsafe { (*now_ty).clone() });
                        return RealValue::Pointer(offset);
                    }
                    // 如果是 get_ptr 也应该存入
                    if let ValueKind::GetPtr(_) = value_data.kind() {
                        println!("get_ptr ?????");
                        let now_ty = GLOBAL_NOW_TYPE.load(Ordering::SeqCst);
                        func_info.insert_type(*self, unsafe { (*now_ty).clone() });
                        return RealValue::Pointer(offset);
                    }

                    return RealValue::StackPos(offset);
                }
            }
        }
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for ValueData {
    type Out = ();

    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) {
        match self.kind() {
            ValueKind::Return(val) => {
                val.generate_asm(buf, program_info);
            }
            ValueKind::Integer(num) => {
                // 应该处理 local 的 integer 只会在 Value里面处理
                // 这里应该要处理一个全局的 integer
                writeln!(buf, "  .word {}", num.value()).unwrap();
            }
            ValueKind::Jump(dest) => {
                dest.generate_asm(buf, program_info);
            }
            ValueKind::Alloc(_alloc) => {
                // 对于 alloc 语句, 应该在一开始计算 stack size 的时候就分配好
                // alloc 做的事情是在 ValueData 里加入了一个新的 Value
            }
            ValueKind::Store(store) => {
                store.generate_asm(buf, program_info);
            }
            ValueKind::Load(load) => {
                load.generate_asm(buf, program_info);
            }
            ValueKind::Binary(binary) => {
                binary.generate_asm(buf, program_info);
            }
            ValueKind::Branch(branch) => {
                branch.generate_asm(buf, program_info);
            }
            ValueKind::Call(call) => {
                call.generate_asm(buf, program_info);
            }
            ValueKind::ZeroInit(zero_init) => {
                zero_init.generate_asm_with_value(buf, program_info, self);
            }
            ValueKind::Aggregate(aggregate) => {
                aggregate.generate_asm(buf, program_info);
            }
            ValueKind::GetElemPtr(get_elem_ptr) => {
                println!("get element ptr1");
                let ty = get_elem_ptr.generate_asm(buf, program_info);
                GLOBAL_NOW_TYPE.store(Box::into_raw(Box::new(ty)), Ordering::SeqCst);
            }
            ValueKind::GetPtr(get_ptr) => {
                println!("get ptr");
                let ty = get_ptr.generate_asm(buf, program_info);
                GLOBAL_NOW_TYPE.store(Box::into_raw(Box::new(ty)), Ordering::SeqCst);
            }
            ValueKind::BlockArgRef(block_arg_ref) => {
                println!("block arg ref");
            }
            ValueKind::FuncArgRef(func_arg_ref) => {
                println!("func arg ref");
            }
            ValueKind::GlobalAlloc(global_alloc) => {
                global_alloc.generate_asm(buf, program_info);
                println!("global alloc");
            }
            _ => unimplemented!(),
        }
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Return {
    type Out = ();

    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) {
        if self.value().is_none() {
            // 没有返回值
            // 直接就返回啦！
            generate_epilogue(buf, program_info);
            return;
        }
        let val = self.value().unwrap();
        let is_unit = program_info
            .get_program()
            .func(*program_info.get_current_func().unwrap().get_func())
            .dfg()
            .value(val)
            .ty()
            .is_unit();
        if is_unit {
            generate_epilogue(buf, program_info);
            return;
        }
        let real_val = val.generate_asm(buf, program_info);

        match real_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li a0, {}", num).unwrap();
            }
            RealValue::StackPos(offset) => {
                load_from_stack(buf, "a0".to_string(), offset);
            }
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv a0, {}", reg).unwrap();
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
                writeln!(buf, "  lw a0, 0(t0)").unwrap();
            }
            _ => {}
        }

        /*函数返回前, 即 ret 指令之前, 你需要生成复原栈指针的指令, 将栈指针加上 S'
         这个过程叫做函数的 epilogue.
        */
        generate_epilogue(buf, program_info);
    }
}

pub fn generate_epilogue(buf: &mut Vec<u8>, program_info: &mut ProgramInfo) {
    let func_info = program_info.get_current_func().unwrap();
    let stack_size = func_info.get_stack_size();

    if stack_size == 0 {
        writeln!(buf, "  ret").unwrap();
        writeln!(buf).unwrap();
        return;
    }

    if func_info.get_call_flag() {
        load_from_stack(buf, "ra".to_string(), (stack_size - 4) as i32);
    }
    if stack_size <= 2047 {
        writeln!(buf, "  addi sp, sp, {}", stack_size).unwrap();
    } else {
        writeln!(buf, "  li t0, {}", stack_size).unwrap();
        writeln!(buf, "  add sp, sp, t0").unwrap();
    }
    writeln!(buf, "  ret").unwrap();
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Load {
    type Out = RealValue;
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let src_val = self.src().generate_asm(buf, program_info);
        let ret = match src_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li t0, {}", num).unwrap();
                RealValue::Reg("t0".to_string())
            }
            RealValue::StackPos(offset) => {
                load_from_stack(buf, "t0".to_string(), offset);
                RealValue::Reg("t0".to_string())
            }
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv t0, {}", reg).unwrap();
                RealValue::Reg("t0".to_string())
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
                writeln!(buf, "  lw t0, 0(t0)").unwrap();
                RealValue::Reg("t0".to_string())
            }
            RealValue::Array(offset) => {
                load_from_stack(buf, "t1".to_string(), offset);
                writeln!(buf, "  lw t0, 0(t1)").unwrap();
                RealValue::Reg("t0".to_string())
            }
            RealValue::Pointer(offset) => {
                load_from_stack(buf, "t1".to_string(), offset);
                writeln!(buf, "  lw t0, 0(t1)").unwrap();
                RealValue::Reg("t0".to_string())
            }
            _ => {
                panic!("Load src is None")
            }
            _ => RealValue::None,
        };
        ret
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Store {
    type Out = ();

    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let dest_val = self.dest().generate_asm(buf, program_info);
        let src_val = self.value().generate_asm(buf, program_info);

        // 先把 src_val 放到 t0 中
        match src_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li t0, {}", num).unwrap();
            }
            RealValue::StackPos(offset) => {
                load_from_stack(buf, "t0".to_string(), offset);
            }
            RealValue::Reg(reg) => {
                if reg != "t0".to_string() {
                    writeln!(buf, "  mv t0, {}", reg).unwrap();
                }
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
                writeln!(buf, "  lw t0, 0(t0)").unwrap();
            }
            RealValue::Pointer(offset) => {
                load_from_stack(buf, "t1".to_string(), offset);
                writeln!(buf, "  lw t0, 0(t1)").unwrap();
            }
            _ => {
                panic!("Store src is None")
            }
            _ => {}
        }

        // 再把 t0 放到 dest_val 中
        match dest_val {
            RealValue::StackPos(offset) => {
                store_to_stack(buf, "t0".to_string(), offset);
            }
            RealValue::Reg(reg) => {
                if reg != "t0".to_string() {
                    writeln!(buf, "  mv {}, t0", reg).unwrap();
                }
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t1, {}", name).unwrap();
                writeln!(buf, "  sw t0, 0(t1)").unwrap();
            }
            RealValue::None => {
                panic!("Store dest is None")
            }
            RealValue::Array(offset) => {
                // 我发现 Array = 一个 Pointer 类似的东西
                load_from_stack(buf, "t1".to_string(), offset);
                writeln!(buf, "  sw t0, 0(t1)").unwrap();
            }
            RealValue::Pointer(offset) => {
                load_from_stack(buf, "t1".to_string(), offset);
                writeln!(buf, "  sw t0, 0(t1)").unwrap();
            }
            _ => {}
        }
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Jump {
    type Out = ();
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let target = self.target().generate_asm(buf, program_info);
        writeln!(buf, "  j {}", target).unwrap();
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Branch {
    type Out = ();
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let cond = self.cond().generate_asm(buf, program_info);
        cond.load_value(buf, program_info, "t0".to_string());
        let tbb = self.true_bb().generate_asm(buf, program_info);
        let fbb = self.false_bb().generate_asm(buf, program_info);
        writeln!(buf, "  beqz t0, {}", fbb).unwrap();
        writeln!(buf, "  j {}", tbb).unwrap();
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Binary {
    type Out = RealValue;
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        let lhs = self.lhs().generate_asm(buf, program_info);
        let rhs = self.rhs().generate_asm(buf, program_info);
        let op = self.op();
        lhs.load_value(buf, program_info, "t0".to_string());
        rhs.load_value(buf, program_info, "t1".to_string());
        match op {
            BinaryOp::Sub => {
                writeln!(buf, "  sub t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Add => {
                writeln!(buf, "  add t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Mul => {
                writeln!(buf, "  mul t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Div => {
                match rhs {
                    RealValue::Const(0) => {
                        panic!("Divide by zero")
                    }
                    _ => {}
                }
                writeln!(buf, "  div t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Mod => {
                match rhs {
                    RealValue::Const(0) => {
                        panic!("Divide by zero")
                    }
                    _ => {}
                }
                writeln!(buf, "  rem t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Eq => {
                writeln!(buf, "  sub t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
                writeln!(buf, "  seqz t0, t0").unwrap();
            }
            BinaryOp::NotEq => {
                writeln!(buf, "  sub t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
                writeln!(buf, "  snez t0, t0").unwrap();
            }
            BinaryOp::Lt => {
                writeln!(buf, "  slt t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Le => {
                writeln!(buf, "  sgt t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
                writeln!(buf, "  seqz t0, t0").unwrap();
            }
            BinaryOp::Gt => {
                writeln!(buf, "  sgt t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Ge => {
                writeln!(buf, "  slt t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
                writeln!(buf, "  seqz t0, t0").unwrap();
            }
            BinaryOp::And => {
                writeln!(buf, "  and t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Or => {
                writeln!(buf, "  or t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Xor => {
                writeln!(buf, "  xor t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Shl => {
                writeln!(buf, "  sll t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Shr => {
                writeln!(buf, "  srl t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
            BinaryOp::Sar => {
                writeln!(buf, "  sra t0, {}, {}", "t0".to_string(), "t1".to_string()).unwrap();
            }
        }
        RealValue::Reg("t0".to_string())
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Call {
    type Out = RealValue;
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        // 存放参数
        let args = self.args();
        let mut arg_count = 0;
        for arg in args.iter() {
            let arg = arg.generate_asm(buf, program_info);
            if arg_count < 8 {
                arg.load_value(buf, program_info, format!("a{}", arg_count));
            } else {
                // 存入栈中
                arg.load_value(buf, program_info, "t0".to_string());
                store_to_stack(buf, "t0".to_string(), (arg_count - 8) * 4);
            }
            arg_count += 1;
        }

        let callee = self.callee().generate_asm(buf, program_info);
        writeln!(buf, "  call {}", callee).unwrap();

        RealValue::Reg("a0".to_string())
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for GlobalAlloc {
    type Out = ();
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &'b mut ProgramInfo<'a>) -> Self::Out {
        program_info
            .get_program()
            .borrow_value(self.init())
            .generate_asm(buf, program_info);
    }
}

impl<'b, 'a> GenerateAsm<'b, 'a> for Aggregate {
    type Out = ();


    /// 这个是为了 GlobalAlloc 服务的
    /// 所以不会碰到 getelemptr
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &mut ProgramInfo) -> Self::Out {
        match self {
            ValueKind::Return(val) => {
                val.generate_asm(buf, program_info);
            }
            ValueKind::Integer(num) => {
                // 应该处理 local 的 integer 只会在 Value里面处理
                // 这里应该要处理一个全局的 integer
                writeln!(buf, "  .word {}", num.value()).unwrap();
            }
            ValueKind::Jump(dest) => {
                dest.generate_asm(buf, program_info);
            }
            ValueKind::Alloc(_alloc) => {
                // 对于 alloc 语句, 应该在一开始计算 stack size 的时候就分配好
                // alloc 做的事情是在 ValueData 里加入了一个新的 Value
            }
            ValueKind::Store(store) => {
                store.generate_asm(buf, program_info);
            }
            ValueKind::Load(load) => {
                load.generate_asm(buf, program_info);
            }
            ValueKind::Binary(binary) => {
                binary.generate_asm(buf, program_info);
            }
            ValueKind::Branch(branch) => {
                branch.generate_asm(buf, program_info);
            }
            ValueKind::Call(call) => {
                call.generate_asm(buf, program_info);
            }
            ValueKind::ZeroInit(_) => {
                
            }
            ValueKind::Aggregate(aggregate) => {
                aggregate.generate_asm(buf, program_info);
            }
            ValueKind::GetElemPtr(_get_elem_ptr) => {
                // 不会到达这里的
            }
            ValueKind::GetPtr(_get_ptr) => {
                // 不会到达这里的
            }
            ValueKind::BlockArgRef(_block_arg_ref) => {
                // 不会到达这里的
            }
            ValueKind::FuncArgRef(_func_arg_ref) => {
                // 不会到达这里的
            }
            ValueKind::GlobalAlloc(global_alloc) => {
                global_alloc.generate_asm(buf, program_info);
                println!("global alloc");
            }
            _ => unimplemented!(),
        };
        writeln!(buf, "  li t2, {}", elem_size).unwrap();
        writeln!(buf, "  mul t1, t1, t2").unwrap();
        writeln!(buf, "  add t0, t0, t1").unwrap();
        RealValue::Reg("t0".to_string())
    }
}

impl GenerateAsmWithValue for ZeroInit {
    type Out = ();

    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &mut ProgramInfo) -> Self::Out {
        for elem in self.elems().iter() {
            let elem_value = elem.generate_asm(buf, program_info);
            match elem_value {
                RealValue::Const(v) => {
                    writeln!(buf, "  .word {}", v).unwrap();
                }
                RealValue::Reg(r) => {
                    let func_info = program_info.get_current_func_mut().unwrap();
                    let offset = func_info.get_current_offset(elem);
                    store_to_stack(buf, r, offset);
                }
                _ => {}
            }
        }
    }
}

impl GenerateAsm for GetElemPtr {
    type Out = Type;
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &mut ProgramInfo) -> Self::Out {
        let ptr = self.src().generate_asm(buf, program_info);
        let index = self.index().generate_asm(buf, program_info);

        // 第一步, 计算 @arr 的地址
        // addi t0, sp, offset
        ptr.calc_arr_addr(buf, program_info, "t0".to_string());
        // 第二步, 计算 getelemptr 的偏移量
        // li t1, index
        // li t2, size
        // mul t1, t1, t2
        // add t0, t0, t1
        index.load_value(buf, program_info, "t1".to_string());
        let func_info = program_info.get_current_func().unwrap();
        let all_ty = match ptr {
            RealValue::Array(_) => {
                func_info.get_alloc_type_kind(&self.src()).unwrap().clone()
            }
            RealValue::StackPos(_) => {
                func_info.get_type(&self.src()).unwrap().clone()
            }
            RealValue::Pointer(_) => {
                func_info.get_type(&self.src()).unwrap().clone()
            }
            RealValue::DataSeg(n) => {
                let ty = program_info.get_value_type(self.src());
                // 得到还是一个指针必须扒皮
                let ty = match ty.kind() {
                    TypeKind::Pointer(b) => b.clone(),
                    _ => ty,
                };
                println!("name: {}, type: {:?}", n, ty);
                ty.clone()
            }
            _ => {
                panic!("GetElemPtr src is None")
            }
        };

        // 有可能是一个 GetPtr 得到的 Array
        let elem_ty = match all_ty.kind() {
            TypeKind::Array(b, _l) => b.clone(),
            TypeKind::Pointer(b) => b.clone(),
            _ => all_ty,
        };
        println!("elem_ty: {:?}", elem_ty);
        writeln!(buf, "  li t2, {}", elem_ty.size()).unwrap();
        writeln!(buf, "  mul t1, t1, t2").unwrap();
        writeln!(buf, "  add t0, t0, t1").unwrap();
        elem_ty.clone()
    }
}


impl GenerateAsm for GetPtr {
    type Out = Type;

    /// GetPtr 和 GetElemPtr 在汇编层面做的完全一样的
    /// GetPtr 不改变 类型
    /// `arr: *[i32, 10]`, 在GetPtr之后得到 `*[i32, 10]`,
    /// 接着进行 GetElemPtr 得到的类型就变成了 `i32`
    fn generate_asm(&self, buf: &mut Vec<u8>, program_info: &mut ProgramInfo) -> Self::Out {
        let ptr = self.src().generate_asm(buf, program_info);
        let index = self.index().generate_asm(buf, program_info);

        // 第一步, 计算 @arr 的地址
        // 这里已经有了, load t0 offset(sp)
        ptr.calc_arr_addr(buf, program_info, "t0".to_string());
        // 第二步, 计算 getptr 的偏移量
        // li t1, index
        // li t2, size
        // mul t1, t1, t2
        // add t0, t0, t1
        index.load_value(buf, program_info, "t1".to_string());
        let func_info = program_info.get_current_func().unwrap();
        let all_ty = program_info.get_program().func(*func_info.get_func()).dfg().value(self.src()).ty();
        

        println!("all_ty: {:?}", all_ty);
        let elem_ty = match all_ty.kind() {
            TypeKind::Array(b, _l) => b.clone(),
            TypeKind::Pointer(b) => b.clone(),
            _ => all_ty.clone(),
        };
        println!("elem_ty: {:?}", elem_ty);
        writeln!(buf, "  li t2, {}", elem_ty.size()).unwrap();
        writeln!(buf, "  mul t1, t1, t2").unwrap();
        writeln!(buf, "  add t0, t0, t1").unwrap();
        all_ty.clone()
    }
}

pub fn insert_type(program_info: &mut ProgramInfo, value: Value, ty: Type) {
    let func_info = program_info.get_current_func_mut().unwrap();
    func_info.insert_type(value, ty.clone());
}

impl RealValue {
    pub fn load_value(
        &self,
        buf: &mut Vec<u8>,
        _program_info: &ProgramInfo,
        reg: String,
    ) -> String {
        match self {
            RealValue::Const(v) => {
                writeln!(buf, "  li {}, {}", reg, v).unwrap();
                reg
            }
            RealValue::Reg(r) => {
                writeln!(buf, "  mv {}, {}", reg, r).unwrap();
                reg
            }
            RealValue::StackPos(offset) => {
                load_from_stack(buf, reg.clone(), *offset);
                reg
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
                writeln!(buf, "  lw {}, 0(t0)", reg).unwrap();
                reg
            }
            RealValue::Pointer(offset) => {
                if *offset <= 2047 && *offset >= -2048 {
                    writeln!(buf, "  addi {}, sp, {}", reg, *offset).unwrap();
                    reg
                } else {
                    writeln!(buf, "  li t0, {}", *offset).unwrap();
                    writeln!(buf, "  add {}, sp, t0", reg).unwrap();
                    reg
                }
            }
            RealValue::None => {
                panic!("RealValue is None")
            }
            RealValue::Array(offset) => {
                println!("Array offset: {}", offset);
                load_from_stack(buf, reg.clone(), *offset);
                reg
            }
            RealValue::Pointer(offset) => {
                load_from_stack(buf, reg.clone(), *offset);
                reg
            }
        }
    }

    pub fn calc_arr_addr(
        &self,
        buf: &mut Vec<u8>,
        _program_info: &ProgramInfo,
        reg: String,
    ) {
        match self {
            RealValue::Array(offset) => {
                if *offset <= 2047 && *offset >= -2048 {
                    writeln!(buf, "  addi {}, sp, {}", reg, offset).unwrap();
                } else {
                    writeln!(buf, "  li t0, {}", offset).unwrap();
                    writeln!(buf, "  add {}, sp, t0", reg).unwrap();
                }
            }
            RealValue::StackPos(offset) => {
                load_from_stack(buf, reg.clone(), *offset);
            }
            RealValue::Pointer(offset) => {
                load_from_stack(buf, reg.clone(), *offset);
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la {}, {}", reg, name).unwrap();
            }
            _ => {}
        }
    }
    
}

pub fn load_from_stack(
    buf: &mut Vec<u8>,
    reg: String,
    offset: i32,
) {
    if offset <= 2047 && offset >= -2048 {
        writeln!(buf, "  lw {}, {}(sp)", reg, offset).unwrap();
    } else {
        writeln!(buf, "  li t1, {}", offset).unwrap();
        writeln!(buf, "  add t1, sp, t1").unwrap();
        writeln!(buf, "  lw {}, 0(t1)", reg).unwrap();
    }   
}

pub fn store_to_stack(
    buf: &mut Vec<u8>,
    reg: String,
    offset: i32,
) {
    if offset <= 2047 && offset >= -2048 {
        writeln!(buf, "  sw {}, {}(sp)", reg, offset).unwrap();
    } else {
        writeln!(buf, "  li t1, {}", offset).unwrap();
        writeln!(buf, "  add t1, sp, t1").unwrap();
        writeln!(buf, "  sw {}, 0(t1)", reg).unwrap();
    }
}
