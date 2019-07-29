#![feature(box_patterns,box_syntax,slice_patterns)]

mod ast;

use ast::{Exp,Op,TEnv,unify};

fn main() {
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Op(
            Op::ADD,
            box Exp::Var("x".to_string()),
            box Exp::Int(1),
        )
    );
    let result = ast.type_inference(&TEnv::None);
    println!("{:?}",result);

}
