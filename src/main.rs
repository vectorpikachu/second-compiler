mod irgenerator;
mod assembler;
mod ast;
use irgenerator::generate_program;
use assembler::generate_riscv;
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::io::Result;
use std::env::args;
use std::fs::read_to_string;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);
fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input);
    let ast = match ast {
        Ok(result) => { result },
        Err(e) => {
            println!("Error: {:?}", e);
            panic!("Parse error.");
        }
    };
    

    let program = generate_program(&ast).unwrap();

    if mode == "-koopa" {
        // 输出到文件
        KoopaGenerator::from_path(output).unwrap().generate_on(&program).unwrap();
    } else if mode == "-riscv" {
        // let mut buf = std::fs::File::create(output).unwrap();
        let mut buf: Vec<u8> = Vec::new();
        generate_riscv(&program, &mut buf);
        let asm = String::from_utf8(buf).unwrap();
        std::fs::write(output, asm).unwrap();
    } else if mode == "-perf" {
        let mut buf: Vec<u8> = Vec::new();
        generate_riscv(&program, &mut buf);
        let asm = String::from_utf8(buf).unwrap();
        std::fs::write(output, asm).unwrap();
        // println!("Performance mode not implemented yet.");
    } else {
        panic!("Unknown mode: {}", mode);
    }
    

    Ok(())
}

