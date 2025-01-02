//! 用来表示内存形式的 risc-v 代码

use std::collections::HashMap;
use std::io::Write;

pub trait ToBuffer {
    fn to_buffer(&self, buf: &mut Vec<u8>);
}

pub trait FromString {
    fn from_string(s: &String) -> Self;
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // 算术和逻辑操作
    And { rd: usize, rs1: usize, rs2: usize },
    Andi { rd: usize, rs1: usize, imm: i32 },
    Add { rd: usize, rs1: usize, rs2: usize },
    Addi { rd: usize, rs1: usize, imm: i32 },
    Sub { rd: usize, rs1: usize, rs2: usize },
    Slt { rd: usize, rs1: usize, rs2: usize },
    Sgt { rd: usize, rs1: usize, rs2: usize },
    Seqz { rd: usize, rs1: usize },
    Snez { rd: usize, rs1: usize },
    Xor { rd: usize, rs1: usize, rs2: usize },
    Xori { rd: usize, rs1: usize, imm: i32 },
    Or { rd: usize, rs1: usize, rs2: usize },
    Ori { rd: usize, rs1: usize, imm: i32 },

    // 移位操作
    Sll { rd: usize, rs1: usize, rs2: usize },
    Srl { rd: usize, rs1: usize, rs2: usize },
    Sra { rd: usize, rs1: usize, rs2: usize },

    // 乘法、除法、余数
    Mul { rd: usize, rs1: usize, rs2: usize },
    Div { rd: usize, rs1: usize, rs2: usize },
    Rem { rd: usize, rs1: usize, rs2: usize },

    // 加载和存储
    Lw { rd: usize, rs1: usize, imm: i32 },
    Sw { rs1: usize, rs2: usize, imm: i32 },

    // 跳转和分支
    Beqz { rs1: usize, label: String },
    Bnez { rs1: usize, label: String },
    J { label: String },
    Call { label: String },
    Ret,

    // 伪指令
    Li { rd: usize, imm: i32 },
    La { rd: usize, label: String },
    Mv { rd: usize, rs: usize },
}

impl Instruction {
    pub fn is_lw(&self) -> bool {
        match self {
            Instruction::Lw { .. } => true,
            _ => false,
        }
    }

    pub fn is_sw(&self) -> bool {
        match self {
            Instruction::Sw { .. } => true,
            _ => false,
        }
    }

    pub fn get_sw_rs1(&self) -> usize {
        match self {
            Instruction::Sw { rs1, .. } => *rs1,
            _ => panic!("Not a sw instruction"),
        }
    }

    pub fn get_sw_rs2(&self) -> usize {
        match self {
            Instruction::Sw { rs2, .. } => *rs2,
            _ => panic!("Not a sw instruction"),
        }
    }

    pub fn get_sw_imm(&self) -> i32 {
        match self {
            Instruction::Sw { imm, .. } => *imm,
            _ => panic!("Not a sw instruction"),
        }
    }

    pub fn get_lw_rd(&self) -> usize {
        match self {
            Instruction::Lw { rd, .. } => *rd,
            _ => panic!("Not a lw instruction"),
        }
    }

    pub fn get_lw_rs1(&self) -> usize {
        match self {
            Instruction::Lw { rs1, .. } => *rs1,
            _ => panic!("Not a lw instruction"),
        }
    }

    pub fn get_lw_imm(&self) -> i32 {
        match self {
            Instruction::Lw { imm, .. } => *imm,
            _ => panic!("Not a lw instruction"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
    pub offset: usize,
    pub instrs: Vec<Instruction>,
}

impl Label {
    pub fn new(name: String, offset: usize, instrs: Vec<Instruction>) -> Self {
        Label { name, offset, instrs }
    }

    pub fn new_with_name(name: String) -> Self {
        Label {
            name,
            offset: 0,
            instrs: Vec::new(),
        }
    }

    pub fn add_instr(&mut self, instr: Instruction) {
        self.instrs.push(instr);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub labels: Vec<Label>,
}

impl Function {
    pub fn new(name: String) -> Self {
        Function {
            name,
            labels: Vec::new(),
        }
    }

    pub fn add_label(&mut self, label: Label) {
        self.labels.push(label);
    }
}

#[derive(Debug, Clone)]
pub struct GloabalData {
    pub name: String,
    pub data: Vec<String>,
}

impl GloabalData {
    pub fn new(name: String) -> Self {
        GloabalData { name, 
            data: Vec::new(), 
        }
    }

    pub fn add_data(&mut self, data: String) {
        self.data.push(data);
    }
}

#[derive(Debug, Clone)]
pub struct AsmProgram {
    pub text_section: Vec<Function>, // 存储程序的汇编代码
    pub data_section: Vec<GloabalData>,         // 数据段，存储数据（如常量等）
}

impl AsmProgram {
    pub fn new() -> Self {
        AsmProgram {
            text_section: Vec::new(),
            data_section: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: Function) {
        self.text_section.push(function);
    }

    pub fn add_data(&mut self, data: GloabalData) {
        self.data_section.push(data);
    }
}

// 寄存器编号到寄存器名称的映射
fn register_to_name(reg_num: usize) -> Option<&'static str> {
    let reg_map: HashMap<usize, &'static str> = [
        (0, "zero"),
        (1, "ra"),
        (2, "sp"),
        (3, "gp"),
        (4, "tp"),
        (5, "t0"),
        (6, "t1"),
        (7, "t2"),
        (8, "s0"),
        (9, "s1"),
        (10, "a0"),
        (11, "a1"),
        (12, "a2"),
        (13, "a3"),
        (14, "a4"),
        (15, "a5"),
        (16, "a6"),
        (17, "a7"),
        (18, "s2"),
        (19, "s3"),
        (20, "s4"),
        (21, "s5"),
        (22, "s6"),
        (23, "s7"),
        (24, "s8"),
        (25, "s9"),
        (26, "s10"),
        (27, "s11"),
        (28, "t3"),
        (29, "t4"),
        (30, "t5"),
        (31, "t6"),
    ]
    .iter()
    .cloned()
    .collect();

    reg_map.get(&reg_num).copied()
}

// 寄存器名称到寄存器编号的映射
fn name_to_register(reg_name: &str) -> Option<usize> {
    let name_map: HashMap<&'static str, usize> = [
        ("zero", 0),
        ("ra", 1),
        ("sp", 2),
        ("gp", 3),
        ("tp", 4),
        ("t0", 5),
        ("t1", 6),
        ("t2", 7),
        ("s0", 8),
        ("fp", 8),
        ("s1", 9),
        ("a0", 10),
        ("a1", 11),
        ("a2", 12),
        ("a3", 13),
        ("a4", 14),
        ("a5", 15),
        ("a6", 16),
        ("a7", 17),
        ("s2", 18),
        ("s3", 19),
        ("s4", 20),
        ("s5", 21),
        ("s6", 22),
        ("s7", 23),
        ("s8", 24),
        ("s9", 25),
        ("s10", 26),
        ("s11", 27),
        ("t3", 28),
        ("t4", 29),
        ("t5", 30),
        ("t6", 31),
    ]
    .iter()
    .cloned()
    .collect();

    name_map.get(reg_name).copied()
}

impl ToBuffer for Instruction {
    fn to_buffer(&self, buf: &mut Vec<u8>) {
        match self {
            Instruction::And { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  and {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Andi { rd, rs1, imm } => {
                writeln!(
                    buf,
                    "  andi {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    imm
                )
                .unwrap();
            }
            Instruction::Add { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  add {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Addi { rd, rs1, imm } => {
                writeln!(
                    buf,
                    "  addi {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    imm
                )
                .unwrap();
            }
            Instruction::Sub { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  sub {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Slt { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  slt {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Sgt { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  sgt {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Seqz { rd, rs1 } => {
                writeln!(
                    buf,
                    "  seqz {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap()
                )
                .unwrap();
            }
            Instruction::Snez { rd, rs1 } => {
                writeln!(
                    buf,
                    "  snez {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap()
                )
                .unwrap();
            }
            Instruction::Xor { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  xor {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Xori { rd, rs1, imm } => {
                writeln!(
                    buf,
                    "  xori {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    imm
                )
                .unwrap();
            }
            Instruction::Or { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  or {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Ori { rd, rs1, imm } => {
                writeln!(
                    buf,
                    "  ori {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    imm
                )
                .unwrap();
            }
            Instruction::Sll { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  sll {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Srl { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  srl {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Sra { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  sra {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Mul { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  mul {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Div { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  div {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Rem { rd, rs1, rs2 } => {
                writeln!(
                    buf,
                    "  rem {}, {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs1).unwrap(),
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Lw { rd, rs1, imm } => {
                writeln!(
                    buf,
                    "  lw {}, {}({})",
                    register_to_name(*rd).unwrap(),
                    imm,
                    register_to_name(*rs1).unwrap()
                )
                .unwrap();
            }
            Instruction::Sw { rs1, rs2, imm } => {
                writeln!(
                    buf,
                    "  sw {}, {}({})",
                    register_to_name(*rs1).unwrap(),
                    imm,
                    register_to_name(*rs2).unwrap()
                )
                .unwrap();
            }
            Instruction::Beqz { rs1, label } => {
                writeln!(
                    buf,
                    "  beqz {}, {}",
                    register_to_name(*rs1).unwrap(),
                    label
                )
                .unwrap();
            }
            Instruction::Bnez { rs1, label } => {
                writeln!(
                    buf,
                    "  bnez {}, {}",
                    register_to_name(*rs1).unwrap(),
                    label
                )
                .unwrap();
            }
            Instruction::J { label } => {
                writeln!(buf, "  j {}", label).unwrap();
            }
            Instruction::Call { label } => {
                writeln!(buf, "  call {}", label).unwrap();
            }
            Instruction::Ret => {
                writeln!(buf, "  ret").unwrap();
            }
            Instruction::Li { rd, imm } => {
                writeln!(buf, "  li {}, {}", register_to_name(*rd).unwrap(), imm).unwrap();
            }
            Instruction::La { rd, label } => {
                writeln!(buf, "  la {}, {}", register_to_name(*rd).unwrap(), label).unwrap();
            }
            Instruction::Mv { rd, rs } => {
                writeln!(
                    buf,
                    "  mv {}, {}",
                    register_to_name(*rd).unwrap(),
                    register_to_name(*rs).unwrap()
                )
                .unwrap();
            }
        }
    }
}

impl ToBuffer for AsmProgram {
    fn to_buffer(&self, buf: &mut Vec<u8>) {
        
        for data in &self.data_section {
            writeln!(buf, "  .data").unwrap();
            writeln!(buf, "  .globl {}", data.name).unwrap();
            writeln!(buf, "{}:", data.name).unwrap();
            for val in &data.data {
                writeln!(buf, "  {}", val).unwrap();
            }
        }

        for func in &self.text_section {
            writeln!(buf, "  .text").unwrap();
            writeln!(buf, "  .globl {}", func.name).unwrap();
            // writeln!(buf, "{}:", func.name).unwrap();
            // 第一个 label 就是函数名
            for label in &func.labels {
                writeln!(buf, "{}:", label.name).unwrap();
                for instr in &label.instrs {
                    instr.to_buffer(buf);
                }
            }
        }
        
    }
}

impl FromString for Instruction {
    fn from_string(s: &String) -> Self {
        let s = s.trim();
        let mut parts = s.split_whitespace().map(|s| s.trim_end_matches(","));
        let instr = parts.next().unwrap();
        match instr {
            "and" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::And {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "andi" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let imm = parts.next().unwrap().parse().unwrap();
                Instruction::Andi {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    imm,
                }
            }
            "add" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Add {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "addi" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let imm = parts.next().unwrap().parse().unwrap();
                Instruction::Addi {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    imm,
                }
            }
            "sub" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Sub {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "slt" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Slt {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "sgt" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Sgt {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "seqz" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                Instruction::Seqz {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                }
            }
            "snez" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                Instruction::Snez {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                }
            }
            "xor" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Xor {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "xori" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let imm = parts.next().unwrap().parse().unwrap();
                Instruction::Xori {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    imm,
                }
            }
            "or" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Or {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "ori" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let imm = parts.next().unwrap().parse().unwrap();
                Instruction::Ori {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    imm,
                }
            }
            "sll" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Sll {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "srl" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Srl {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "sra" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Sra {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                   rs2: rs2.unwrap(),
                }
            }
            "mul" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Mul {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "div" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Div {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "rem" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2 = name_to_register(parts.next().unwrap());
                Instruction::Rem {
                    rd: rd.unwrap(),
                    rs1: rs1.unwrap(),
                    rs2: rs2.unwrap(),
                }
            }
            "lw" => {
                let rd = name_to_register(parts.next().unwrap());
                let mut parts = parts.next().unwrap().split("(");
                let imm = parts.next().unwrap().parse().unwrap();
                let rs1 = name_to_register(parts.next().unwrap().trim_end_matches(")")).unwrap();
                Instruction::Lw {
                    rd: rd.unwrap(),
                    rs1: rs1,
                    imm,
                }
            }
            "sw" => {
                let rs1 = name_to_register(parts.next().unwrap());
                let rs2_offset = parts.next().unwrap();
                let mut parts = rs2_offset.split("(");
                let imm = parts.next().unwrap().parse().unwrap();
                let rs2 = name_to_register(parts.next().unwrap().trim_end_matches(")")).unwrap();
                Instruction::Sw {
                    rs1: rs1.unwrap(),
                    rs2: rs2,
                    imm,
                }
            }
            "beqz" => {
                let rs1 = name_to_register(parts.next().unwrap());
                let label = parts.next().unwrap();
                Instruction::Beqz {
                    rs1: rs1.unwrap(),
                    label: label.to_string(),
                }
            }
            "bnez" => {
                let rs1 = name_to_register(parts.next().unwrap());
                let label = parts.next().unwrap();
                Instruction::Bnez {
                    rs1: rs1.unwrap(),
                    label: label.to_string(),
                }
            }
            "j" => {
                let label = parts.next().unwrap().to_string();
                Instruction::J { label }
            }
            "call" => {
                let label = parts.next().unwrap().to_string();
                Instruction::Call { label }
            }
            "ret" => Instruction::Ret,
            "li" => {
                let rd = name_to_register(parts.next().unwrap());
                let imm = parts.next().unwrap().parse().unwrap();
                Instruction::Li {
                    rd: rd.unwrap(),
                    imm,
                }
            }
            "la" => {
                let rd = name_to_register(parts.next().unwrap());
                let label = parts.next().unwrap().to_string();
                Instruction::La { rd: rd.unwrap(), label }
            }
            "mv" => {
                let rd = name_to_register(parts.next().unwrap());
                let rs = name_to_register(parts.next().unwrap());
                Instruction::Mv {
                    rd: rd.unwrap(),
                    rs: rs.unwrap(),
                }
            }
            _ => panic!("Invalid instruction"),
        }
    }
}
