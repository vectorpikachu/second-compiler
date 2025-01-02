//! 实现窥孔优化(Peephole Optimization)
//! 现在实现最简单的, 删除不需要的 lw

use super::asm_struct::{AsmProgram, FromString, Function, GloabalData, Instruction, Label};

pub enum Section {
    Text,
    Data,
}

pub trait PeepholeOpt {
    fn peephole_opt(&self) -> Self;  
}

/// 把传入的 assembly code 翻译成 Instruction 的 Vec
pub fn parse_assembly(code: &str) -> AsmProgram {
    
    let mut program = AsmProgram::new();
    let mut current_function: Option<Function> = None;
    let mut current_label = None;
    let mut current_section = None;
    let mut current_data: Option<GloabalData> = None;
    let mut inst_count = 0;
    for line in code.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with(".text") {
            if let Some(label) = current_label.take() {
                if let Some(func) = current_function.as_mut() {
                    func.add_label(label);
                }
            }
            if let Some(func) = current_function.as_mut() {
                program.add_function(func.clone());
            }
            current_section = Some(Section::Text);
        } else if line.starts_with(".data") {
            if let Some(data) = current_data.take() {
                program.add_data(data);
            }
            current_section = Some(Section::Data);
        } else if line.starts_with(".globl") {
            match current_section {
                Some(Section::Text) => {
                    let func_name = line.trim_start_matches(".globl ").to_string();
                    current_function = Some(Function::new(func_name));
                }
                Some(Section::Data) => {
                    let data_name = line.trim_start_matches(".globl ").to_string();
                    current_data = Some(GloabalData::new(data_name));
                }
                None => {
                    panic!("No section specified");
                }
            }   
        } else if line.ends_with(":") {
            // 标签
            let label_name = line.trim_end_matches(":").to_string();
            match current_section {
                Some(Section::Text) => {
                    if let Some(label) = current_label.take() {
                        if let Some(func) = current_function.as_mut() {
                            func.add_label(label);
                        }
                    }
                    current_label = Some(Label::new(label_name, inst_count, Vec::new()));
                }
                Some(Section::Data) => {
                    // Do nothing
                }
                None => {
                    panic!("No section specified");
                }
            }
        } else if line.starts_with(".word") || line.starts_with(".zero") {
            let data_str = line.to_string();
            if let Some(data) = current_data.as_mut() {
                data.add_data(data_str);
            }
        } else {
            // 指令
            let inst = Instruction::from_string(&line.to_string());
            match current_section {
                Some(Section::Text) => {
                    if let Some(label) = current_label.as_mut() {
                        label.add_instr(inst.clone());
                    }
                    inst_count += 1;
                }
                Some(Section::Data) => {
                    // Do nothing
                }
                None => {
                    panic!("No section specified");
                }
            }
        }
    }

    if let Some(label) = current_label.take() {
        if let Some(func) = current_function.as_mut() {
            func.add_label(label);
        }
    }

    if let Some(func) = current_function {
        program.add_function(func);
    }

    if let Some(data) = current_data {
        program.add_data(data);
    }
    program
}

impl PeepholeOpt for AsmProgram {

    /// 我在这里实现了一个简单的窥孔优化
    /// 只要发现后一条 lw 用到了前一条 sw 的结果, 并且目的寄存器和 sw 的源寄存器一致, 
    /// 你就可以直接把 lw 删掉.
    /// 或者使用 mv 来代替
    fn peephole_opt(&self) -> Self {

        let mut instr_id = 0;
        let mut new_program = AsmProgram::new();
        for func in self.text_section.iter() {
            let mut new_func = Function::new(func.name.clone());
            for label in func.labels.iter() {
                let mut new_label = Label::new_with_name(label.name.clone());
                new_label.offset = instr_id;
                for (i, inst) in label.instrs.iter().enumerate() {
                    if i >= 1 {
                        let prev_inst = &label.instrs[i - 1];
                        if prev_inst.is_sw() && inst.is_lw() {
                            // 删除不需要的 lw
                            let sw_rs1 = prev_inst.get_sw_rs1();
                            let sw_rs2 = prev_inst.get_sw_rs2();
                            let sw_imm = prev_inst.get_sw_imm();
                            let lw_rd = inst.get_lw_rd();
                            let lw_rs1 = inst.get_lw_rs1();
                            let lw_imm = inst.get_lw_imm();
                            if sw_rs1 == lw_rd && sw_rs2 == lw_rs1 && sw_imm == lw_imm {
                                continue;
                            }
                            if sw_rs1 != lw_rd && sw_rs2 == lw_rs1 && sw_imm == lw_imm {
                                let instr = Instruction::Mv { rd: lw_rd, rs: sw_rs1 };
                                new_label.add_instr(instr);
                                instr_id += 1;
                                continue;
                            }
                        }
                    }
                    new_label.add_instr(inst.clone());
                    instr_id += 1;
                }
                new_func.add_label(new_label);
            }
            new_program.add_function(new_func);
        }

        for data in self.data_section.iter() {
            new_program.add_data(data.clone());
        }

        new_program
    }
}
