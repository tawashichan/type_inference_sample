#![feature(box_patterns,box_syntax,slice_patterns)]

mod ast;

use ast::{Exp,Op,TEnv,unify};

fn main() {
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Op(
            Op::ADD,
            box Exp::Int(1),
            box Exp::Var("x".to_string()),
        )
    );
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Func(
            "y".to_string(),
            box Exp::Op(
                Op::ADD,
                box Exp::Op(
                    Op::ADD,
                    box Exp::Var("x".to_string()),
                    box Exp::Int(1),
                ),
                box Exp::Var("y".to_string()),
            )
        )
    );
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Func(
            "y".to_string(),
            box Exp::Op(
                Op::ADD,
                box Exp::Var("x".to_string()),
                box Exp::Var("y".to_string()),
            )
        )
    );
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::If(
            box Exp::Var("x".to_string()),
            box Exp::Int(1),
            box Exp::Int(2),
        )
    );
    let ast = Exp::LetRec(
        "x".to_string(),
        "y".to_string(),
        box Exp::If(
            box Exp::Bool(true),
            box Exp::App(box Exp::Var("x".to_string()),box Exp::Int(1)),
            box Exp::Int(1),
        ),
        box Exp::App(box Exp::Var("x".to_string()),box Exp::Int(1)),
    );
    println!("{:?}",ast);
    let result = ast.type_inference(&TEnv::None);
    println!("{:?}",result);

}
