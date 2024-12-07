
/* 全局的ast */

/// 代表一个Compilation Unit, 里面含有多个GlobalEntry = (Decl | FuncDef)
#[derive(Debug)]
pub struct CompUnit {
    pub entries: Vec<GlobalEntry>,
}

/// 代表一个全局的声明, 全局变量和函数声明
#[derive(Debug)]
pub enum GlobalEntry {
    Decl(Decl),
    FuncDef(FuncDef),
}

/* Declarations */

/// 变量和常量定义, Decl ::= ConstDecl | VarDecl;
#[derive(Debug)]
pub enum Decl {
    Var(VarDecl),
    Const(ConstDecl),
}

/// 常量定义
/// ConstDecl ::= "const" BType ConstDef {"," ConstDef} ";"
#[derive(Debug)]
pub struct ConstDecl {
    pub ty: BType,
    pub defs: Vec<ConstDef>,
}

/// ConstDef ::= IDENT {"[" ConstExp "]"} "=" ConstInitVal;
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub array_index: Vec<ConstExp>,
    pub init_val: ConstInitVal,
}

/// ConstInitVal ::= ConstExp | "{" [ConstInitVal {"," ConstInitVal}] "}";
#[derive(Debug)]
pub enum ConstInitVal {
    Exp(ConstExp),
    Array(Vec<ConstInitVal>),
}

/// 变量定义
/// VarDecl ::= BType VarDef {"," VarDef} ";"
#[derive(Debug)]
pub struct VarDecl {
    pub ty: BType,
    pub defs: Vec<VarDef>,
}

/// VarDef ::= IDENT {"[" ConstExp "]"} | IDENT {"[" ConstExp "]"} "=" InitVal
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub array_index: Vec<ConstExp>,
    pub init_val: Option<InitVal>,
}

/// InitVal ::= Exp | "{" [InitVal {"," InitVal}] "}";
#[derive(Debug)]
pub enum InitVal {
    Exp(Exp),
    Array(Vec<InitVal>),
}

/// 函数定义
/// FuncDef ::= FuncType IDENT "(" [FuncFParams] ")" Block
#[derive(Debug)]
pub struct FuncDef {
    pub ty: FuncType,
    pub ident: String,
    pub params: Option<FuncFParams>,
    pub block: Block,
}

/// FuncFParams ::= FuncFParam {"," FuncFParam}
#[derive(Debug)]
pub struct FuncFParams {
    pub params: Vec<FuncFParam>,
}

/// FuncFParam ::= BType IDENT ["[" "]" {"[" ConstExp "]"}]
#[derive(Debug)]
pub struct FuncFParam {
    pub ty: BType,
    pub ident: String,
    pub array_index: Option<Vec<ConstExp>>,
}

/* Statements */

/// 语句块
/// Block ::= "{" {BlockItem} "}"
#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

/// BlockItem ::= Decl | Stmt
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

/// Stmt ::= LVal "=" Exp ";" | [Exp] ";" | Block | "if" "(" Exp ")" Stmt ["else" Stmt]
/// | "while" "(" Exp ")" Stmt | "break" ";" | "continue" ";" | "return" [Exp] ";";
#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Exp),
    Exp(Option<Exp>),
    Block(Block),
    If(Exp, Box<Stmt>, Option<Box<Stmt>>),
    While(Exp, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Exp>),
}

/* Expressions */

/// Exp ::= LOrExp
#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

/// LVal = IDENT {"[" Exp "]"}
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub array_index: Vec<Exp>,
}

/// PrimaryExp ::= "(" Exp ")" | LVal | Number
#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

/// UnaryExp ::= PrimaryExp | IDENT {"(" [FuncRParams] ")"}| UnaryOp UnaryExp
#[derive(Debug)]
pub enum UnaryExp {
    Primary(PrimaryExp),
    Call(String, Option<FuncRParams>),
    Unary(UnaryOp, Box<UnaryExp>),
}

/// UnaryOp ::= "+" | "-" | "!"
#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

/// FuncRParams ::= Exp {"," Exp}
#[derive(Debug)]
pub struct  FuncRParams {
    pub params: Vec<Exp>,
}

/// MulExp ::= UnaryExp | MulExp MulOp UnaryExp
#[derive(Debug)]
pub enum MulExp {
    Unary(UnaryExp),
    Mul(Box<MulExp>, MulOp, Box<UnaryExp>),
}

/// MulOp ::= "*" | "/" | "%"
#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

/// AddExp ::= MulExp | AddExp AddOp MulExp
#[derive(Debug)]
pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, AddOp, Box<MulExp>),
}

/// AddOp ::= "+" | "-"
#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

/// RelExp ::= AddExp | RelExp RelOp AddExp
#[derive(Debug)]
pub enum RelExp {
    Add(AddExp),
    Rel(Box<RelExp>, RelOp, Box<AddExp>),
}

/// RelOp ::= "<" | ">" | "<=" | ">="
#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}


#[derive(Debug)]/// EqExp ::= RelExp | EqExp EqOp RelExp
pub enum EqExp {
    Rel(RelExp),
    Eq(Box<EqExp>, EqOp, Box<RelExp>),
}

/// EqOp ::= "==" | "!="
#[derive(Debug)]
pub enum EqOp {
    Eq,
    Ne,
}

/// LAndExp ::= EqExp | LAndExp "&&" EqExp
#[derive(Debug)]
pub enum LAndExp {
    Eq(EqExp),
    And(Box<LAndExp>, AndOp, Box<EqExp>),
}

#[derive(Debug)]
pub enum AndOp {
    And,
}

/// LOrExp ::= LAndExp | LOrExp "||" LAndExp
#[derive(Debug)]
pub enum LOrExp {
    And(LAndExp),
    Or(Box<LOrExp>, OrOp, Box<LAndExp>),
}

#[derive(Debug)]
pub enum OrOp {
    Or,
}

/// ConstExp ::= Exp
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}

/* Types */

/// BType ::= "int"
#[derive(Debug)]
pub enum BType {
    Int,
}

/// FuncType ::= "int" | "void"
#[derive(Debug, PartialEq)]
pub enum FuncType {
    Int,
    Void,
}
