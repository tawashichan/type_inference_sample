#![feature(box_patterns,box_syntax)]

mod ast;

use ast::{Exp,Op,TEnv};

fn main() {
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Op(
            Op::ADD,
            box Exp::Var("x".to_string()),
            box Exp::Int(1),
        )
    );
    let (r,typ) = ast.extract(&TEnv::None);
    println!("{:?} {:?}",r,typ);
}
