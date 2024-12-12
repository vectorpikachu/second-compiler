//! 表达式求值

use koopa::ir::Value;

use crate::ast::*;
use super::scopes::*;

#[derive(Debug, Clone)]
pub enum ExpResult {
    Void,
    Int(Value),
    ArrayPtr(Value)
}

impl ExpResult {
    pub fn unwrap_int(&self) -> Value {
        match self {
            ExpResult::Int(value) => *value,
            ExpResult::ArrayPtr(p) => *p,
            _ => unimplemented!(),
        }
    }
}

impl ConstExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        self.exp.evaluate(scopes)
    }
}

impl Exp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        self.lor_exp.evaluate(scopes)
    }
}

impl LOrExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            LOrExp::And(and_exp) => and_exp.evaluate(scopes),
            LOrExp::Or(left, _, right) => {
                let left_value = left.evaluate(scopes);
                // 一种短路求值的方式
                if left_value != 0 {
                    1
                } else {
                    let right_value = right.evaluate(scopes);
                    if right_value != 0 {1} else {0}
                }
            }
        }
    }
}

impl LAndExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.evaluate(scopes),
            LAndExp::And(left, _, right) => {
                let left_value = left.evaluate(scopes);
                // 一种短路求值的方式
                if left_value == 0 {
                    0
                } else {
                    let right_value = right.evaluate(scopes);
                    if right_value == 0 {0} else {1}
                }
            }
        }
    }
}

impl EqExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            EqExp::Rel(rel_exp) => rel_exp.evaluate(scopes),
            EqExp::Eq(left, op, right) => {
                let left_value = left.evaluate(scopes);
                let right_value = right.evaluate(scopes);
                match op {
                    EqOp::Eq => {
                        if left_value == right_value {
                            1
                        } else {
                            0
                        }
                    }
                    EqOp::Ne => {
                        if left_value != right_value {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
        }
    }
}

impl RelExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            RelExp::Add(add_exp) => add_exp.evaluate(scopes),
            RelExp::Rel(left, op, right) => {
                let left_value = left.evaluate(scopes);
                let right_value = right.evaluate(scopes);
                match op {
                    RelOp::Lt => {
                        if left_value < right_value {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Gt => {
                        if left_value > right_value {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Le => {
                        if left_value <= right_value {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Ge => {
                        if left_value >= right_value {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
        }
    }
}

impl AddExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            AddExp::Mul(mul_exp) => mul_exp.evaluate(scopes),
            AddExp::Add(left, op, right) => {
                let left_value = left.evaluate(scopes);
                let right_value = right.evaluate(scopes);
                match op {
                    AddOp::Add => left_value + right_value,
                    AddOp::Sub => left_value - right_value,
                }
            }
        }
    }
}

impl MulExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            MulExp::Unary(unary) => unary.evaluate(scopes),
            MulExp::Mul(left, op, right) => {
                let left_value = left.evaluate(scopes);
                let right_value = right.evaluate(scopes);
                match op {
                    MulOp::Mul => left_value * right_value,
                    MulOp::Div => left_value / right_value,
                    MulOp::Mod => left_value % right_value,
                }
            }
        }
    }
}

impl UnaryExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            UnaryExp::Primary(primary) => primary.evaluate(scopes),
            UnaryExp::Unary(op, exp) => {
                let value = exp.evaluate(scopes);
                match op {
                    UnaryOp::Pos => value,
                    UnaryOp::Neg => -value,
                    UnaryOp::Not => if value != 0 {0} else {1},
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl PrimaryExp {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.evaluate(scopes),
            PrimaryExp::LVal(lval) => lval.evaluate(scopes),
            PrimaryExp::Number(num) => *num,
        }
    }
}

impl LVal {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        if let Some(num) = scopes.get_value(&self.ident) {
            match num {
                VarValue::Const(num) => *num,
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }
}

/// 只是对于单个的表达式求值
impl InitVal {
    pub fn evaluate(&self, scopes: &Scopes) -> i32 {
        match self {
            InitVal::Exp(exp) => exp.evaluate(scopes),
            _ => {
                unimplemented!()
            }
        }
    }
}
