# Compiler

## 安装Docker

```shell
docker pull maxxing/compiler-dev
```

## 整体的架构

sysy.lalrpop 放着词法分析的.

运行的命令行：
```shell
cargo run -- -koopa hello.c -o hello.koopa
```

现在加入gitlab.

```shell
autotest -koopa -s lv3 /root/compiler
```

如果放的是%n, 实际上是一个Value, 否则可以在dfg中找到.

```
docker run -it --rm -v D:\HuaweiMoveData\Users\平面向皮卡丘\Desktop\compilers\second-compiler:/root/compiler maxxing/compiler-dev autotest -riscv -s lv1 /root/compiler
```

```
docker run -it --rm -v D:\HuaweiMoveData\Users\平面向皮卡丘\Desktop\compilers\second-compiler:/root/compiler maxxing/compiler-dev autotest -koopa -s lv1 /root/compiler
```

## Dangling-else 的解决方法

```
Stmt ::= if Exp Stmt [else Stmt] | ...
```

转化为

```
Stmt ::= OpenStmt | ClosedStmt
ClosedStmt ::= if Exp ClosedStmt else ClosedStmt | BasicStmt
OpenStmt ::= if Exp Stmt | if Exp ClosedStmt else OpenStmt
BasicStmt ::= Resturn | Assgn | Exp | Block
```

