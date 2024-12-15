//! 生成 RISCV 的汇编代码

use koopa::ir::entities::ValueData;
use koopa::ir::values::Return;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value, ValueKind};

use super::function_info::FunctionInfo;
use super::program_info::ProgramInfo;
use std::io::Write;

pub trait GenerateAsm {
    // 这样就可以返回什么乱写了
    type Out;
    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) -> Self::Out;
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
        writeln!(buf, "  .text").unwrap();
        let func_name = self.name().strip_prefix("@").unwrap();
        writeln!(buf, "  .globl {}", func_name).unwrap();
        writeln!(buf, "{}:", func_name).unwrap();

        let func_info = program_info.get_current_func_mut().unwrap();
        
        // 应该先把 函数名加入到函数信息中
        for (bb, bb_data) in self.dfg().bbs() {
            func_info.set_bb_name(*bb, bb_data.name().clone());
        }

        // BasicBlock - A handle of Koopa IR basic block.
        // BasicBlockNode - insts: KeyNodeList<Value, InstNode, InstMap>,
        for (bb, node) in self.layout().bbs() {
            let bb_name = bb.generate_asm(buf, program_info);
            // 不能直接写在这里, 会引用可变引用两次
            writeln!(buf, "{}:", bb_name);
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
    type Out = ();

    fn generate_asm(&self,
        buf: &mut Vec<u8>,
        program_info: &mut ProgramInfo,
    ) {
        // writeln!(buf,"  {}:", inst_name).unwrap();
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
                writeln!(buf, "  j 1").unwrap();
            }
            ValueKind::Alloc(alloc) => {
                writeln!(buf, "  mv a0, x").unwrap();
            }
            ValueKind::Store(dest) => {
                writeln!(buf, "  mv a0, x").unwrap();
            }
            ValueKind::Load(load) => {
                writeln!(buf, "  mv a0, x").unwrap();
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
        
        let func_info = program_info.get_current_func().unwrap();



        writeln!(buf, "  li a0, 0").unwrap();

        writeln!(buf, "  ret").unwrap();

    }
}