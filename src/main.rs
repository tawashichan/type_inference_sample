#![feature(box_patterns,box_syntax,slice_patterns)]

mod ast;
mod lexer;
mod parser;

use ast::{Exp,Op,TEnv,unify};

fn main() {
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Op(
            Op::Plus,
            box Exp::Int(1),
            box Exp::Var("x".to_string()),
        )
    );
    let ast = Exp::Func(
        "x".to_string(),
        box Exp::Func(
            "y".to_string(),
            box Exp::Op(
                Op::Plus,
                box Exp::Op(
                    Op::Plus,
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
                Op::Plus,
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
    let code = "let rec fib = fun y -> if y = 0 then 0 else if y = 1 then 1 else fib (y - 1) + fib (y - 2) in fib 5";
    let tokens = lexer::str_to_tokens(code);
    let ast = parser::parse(&tokens);

    println!("{:?}",ast);
    let result = ast.type_inference(&TEnv::None);
    println!("{:?}",result);

}
