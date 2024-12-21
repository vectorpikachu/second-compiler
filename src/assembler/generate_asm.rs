//! 生成 RISCV 的汇编代码

use koopa::ir::entities::ValueData;
use koopa::ir::values::{Load, Return, Store,Jump};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value, ValueKind};

use super::function_info::FunctionInfo;
use super::program_info::ProgramInfo;
use std::io::Write;

use super::function_info::RealValue;

/// 把所有的局部变量都放在栈上 + 寄存器上

pub trait GenerateAsm {
    // 这样就可以返回什么乱写了
    type Out;
    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out;
}

pub trait GenerateOffset {
    fn generate_offset(&self, program_info: &mut ProgramInfo) -> i32;
}

impl GenerateAsm for Program {
    type Out = ();
    // 生成整个程序的汇编代码
    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) {
        
        // 我们对于当前的每个函数都生成汇编代码
        for &func in self.func_layout() {
            let func_info = FunctionInfo::new(func);
            program_info.set_current_func(func_info);
            self.func(func).generate_asm(buf, program_info);
        }
    }
}

impl GenerateAsm for Function {
    type Out = String;
    fn generate_asm(&self,
        _buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        program_info.get_program().func(*self).name().to_string()
    }
}

/// 生成函数的 prologue
pub fn generate_prologue(buf: &mut Vec<u8>, 
    program_info: &mut ProgramInfo,
    function_data: &FunctionData) {
    writeln!(buf, "  .text").unwrap();
    let func_name = function_data.name().strip_prefix("@").unwrap();
    writeln!(buf, "  .globl {}", func_name).unwrap();
    writeln!(buf, "{}:", func_name).unwrap();
    let func_info = program_info.get_current_func_mut().unwrap();
        
    // 应该先把 函数名加入到函数信息中
    for (bb, bb_data) in function_data.dfg().bbs() {
        func_info.set_bb_name(*bb, bb_data.name().clone());
    }

    // 计算要分配的栈空间.
    let mut stack_size = 0;

    for (_bb, node) in function_data.layout().bbs() {
        for inst in node.insts().keys() {
            let value = function_data.dfg().value(*inst);
            if !value.ty().is_unit() {
                stack_size += 4;
            }
        }
    }
    // 对齐到 16 字节
    stack_size = (stack_size + 15) / 16 * 16;
    func_info.set_stack_size(stack_size as usize);
    // 在函数入口处, 生成更新栈指针的指令, 将栈指针减去 StackSize. 这个过程叫做函数的 prologue.
    
    // 应该在计算完栈大小之后就为每个变量分配位置
    
    if stack_size <= 2048 {
        writeln!(buf, "  addi sp, sp, -{}", stack_size).unwrap();
    } else {
        writeln!(buf, "  li t0, -{}", stack_size).unwrap();
        writeln!(buf, "  add sp, sp, t0").unwrap();
    }
}

impl GenerateAsm for FunctionData {
    type Out = ();
    fn generate_asm(&self,
            buf: &mut Vec<u8>,
            program_info: &mut ProgramInfo,
        ) {
        /*
          Koopa IR 中, 函数声明是一种特殊的函数, 它们和函数定义是放在一起的. 
          也就是说, 在上一节的基础上, 
          你需要在扫描函数时跳过 Koopa IR 中的所有函数声明.
         */
        // 没有进入块, 这就是一个声明了
        if self.layout().entry_bb().is_none() {
            return;
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
                self.dfg().value(*inst).generate_asm(buf, program_info);
            }
        }
    }
}

impl GenerateAsm for BasicBlock {
    type Out = String;
    fn generate_asm(&self,
        _buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        let func_info = program_info.get_current_func().unwrap();
        let bb_name = func_info.get_bb_name(*self);
        bb_name
    }
}


impl GenerateAsm for Value {
    type Out = RealValue;

    /// 生成 Value 的汇编代码
    /// 产出是一个 RealValue, 用来表示这个 Value 的位置
    fn generate_asm(&self,
        _buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        // writeln!(buf,"  {}:", inst_name).unwrap();
        if self.is_global() {
            // 全局变量放在 .data 段
            return RealValue::DataSeg(program_info.get_value_name(*self).unwrap());
        } else {
            let func_info = program_info.get_current_func().unwrap();
            let value_data = {
                let program = program_info.get_program();
                program.func(*func_info.get_func()).dfg().value(*self).clone()
            };
            match value_data.kind() {
                ValueKind::Integer(num) => {
                    return RealValue::Const(num.value());
                }
                ValueKind::Load(load) => {
                    return load.generate_asm(_buf, program_info);
                }
                _ => {
                    let func_info = program_info.get_current_func_mut().unwrap();
                    let offset = func_info.get_current_offset(self);
                    return RealValue::StackPos(offset);
                }
            }
        }
    }
}

impl GenerateAsm for ValueData {
    type Out = ();

    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) {
        match self.kind() {
            ValueKind::Return(val) => {
                val.generate_asm(buf, program_info);
            }
            ValueKind::Integer(num) => {
                writeln!(buf, "  li a0, {}", num.value()).unwrap();
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
            ValueKind::Load(_load) => {
                
            }
            _ => unimplemented!()
        }
    }
}

impl GenerateAsm for Return {
    type Out = ();

    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) {

        let val = self.value().unwrap();
        let is_unit = program_info.get_program().func(*program_info.get_current_func().unwrap().get_func()).dfg().value(val).ty().is_unit();
        if is_unit {
            generate_epilogue(buf, program_info);
            return;
        }
        let real_val = val.generate_asm(buf, program_info);
        let func_info = program_info.get_current_func().unwrap();
        
        println!("{:?}", func_info.get_allocs());
        
        println!("{:?}", real_val);

        match real_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li a0, {}", num).unwrap();
            }
            RealValue::StackPos(offset) => {
                writeln!(buf, "  lw a0, {}(sp)", offset).unwrap();
            }
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv a0, {}", reg).unwrap();
            }
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la a0, {}", name).unwrap();
            }
            RealValue::None => {}
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
    if stack_size <= 2047 {
        writeln!(buf, "  addi sp, sp, {}", stack_size).unwrap();
    } else {
        writeln!(buf, "  li t0, {}", stack_size).unwrap();
        writeln!(buf, "  add sp, sp, t0").unwrap();
    }
    writeln!(buf, "  ret").unwrap();
}

impl GenerateAsm for Load {
    type Out = RealValue;
    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        let src_val = self.src().generate_asm(buf, program_info);
        let ret = match src_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li t0, {}", num).unwrap();
                RealValue::Reg("t0".to_string())
            },
            RealValue::StackPos(offset) => {
                writeln!(buf, "  lw t0, {}(sp)", offset).unwrap();
                RealValue::Reg("t0".to_string())
            },
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv t0, {}", reg).unwrap();
                RealValue::Reg("t0".to_string())
            },
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
                RealValue::Reg("t0".to_string())
            },
            RealValue::None => {
                panic!("Load src is None")
            }
        };
        ret
    }
}

impl GenerateAsm for Store {
    type Out = ();

    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        let dest_val = self.dest().generate_asm(buf, program_info);
        let src_val = self.value().generate_asm(buf, program_info);
        // 先把 src_val 放到 t0 中
        match src_val {
            RealValue::Const(num) => {
                writeln!(buf, "  li t0, {}", num).unwrap();
            },
            RealValue::StackPos(offset) => {
                writeln!(buf, "  lw t0, {}(sp)", offset).unwrap();
            },
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv t0, {}", reg).unwrap();
            },
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la t0, {}", name).unwrap();
            },
            RealValue::None => {
                panic!("Store src is None")
            }
        }

        // 再把 t0 放到 dest_val 中
        match dest_val {
            RealValue::StackPos(offset) => {
                writeln!(buf, "  sw t0, {}(sp)", offset).unwrap();
            },
            RealValue::Reg(reg) => {
                writeln!(buf, "  mv {}, t0", reg).unwrap();
            },
            RealValue::DataSeg(name) => {
                writeln!(buf, "  la {}, t0", name).unwrap();
            },
            RealValue::None => {
                panic!("Store dest is None")
            }
            _ => {}
        }
    }
}

impl GenerateAsm for Jump {
    type Out = ();
    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out {
        let target = self.target().generate_asm(buf, program_info);
        writeln!(buf, "  j {}", target).unwrap();
    }
}