use crate::lexer::Token;
use crate::ast::*;

pub fn parse(tokens: &[Token]) -> Exp {
    match parse_exp(tokens) {
        Ok((exp,rest)) => {
            match rest {
                &[] => exp,
                _ => panic!("exp: {:?} \nrest: {:?}", exp, rest),
            }
        }
        Err(e) => panic!(e)
    }
}

fn parse_exp(tokens: &[Token]) -> Result<(Exp,&[Token]),String> {
    let (exp, rest) = parse_op_exp(tokens)?;
    match rest {
        [Token::PLUS, res..] => {
            let (ex, re) = parse_exp(res)?;
            Ok((Exp::Op(Op::Plus, box exp, box ex), re))
        }
        [Token::LET, Token::VAR(s), Token::EQUAL, rest..] => parse_exp(rest),
        [Token::RPAR, res..] => Ok((exp, rest)),
        [Token::IN, res..] => Ok((exp, rest)),
        _ => Ok((exp, rest)),
    }
}

fn parse_op_exp(tokens: &[Token]) -> Result<(Exp, &[Token]),String> {
    let (exp, rest) = parse_term(tokens)?;
    match rest {
        [Token::LT, res..] => {
            let (ex, re) = parse_op_exp(res)?;
            Ok((Exp::Op(Op::Lt, box exp, box ex), re))
        }
        [Token::EQUAL,res..] => {
            let (ex, re) = parse_op_exp(res)?;
            Ok((Exp::Op(Op::Equal, box exp, box ex), re))
        }
        [Token::MINUS,res..] => {
            let (ex, re) = parse_op_exp(res)?;
            Ok((Exp::Op(Op::Minus, box exp, box ex), re))
        }
        _ => Ok(parse_app(exp, rest)),
    }
}

fn parse_term(tokens: &[Token]) -> Result<(Exp, &[Token]),String> {
    match tokens {
        [Token::LPAR, rest..] => {
            let (exp, res) = parse_exp(rest)?;
            match res {
                [Token::RPAR, re..] => Ok((exp, re)),
                _ => panic!("{:?}", res),
            }
        }
        [Token::INT(i), rest..] => Ok((Exp::Int(*i), rest)),
        [Token::VAR(s), rest..] => Ok((Exp::Var(s.clone()), rest)),
        [Token::TRUE, rest..] => Ok((Exp::Bool(true), rest)),
        [Token::FALSE, rest..] => Ok((Exp::Bool(false), rest)),
        [Token::LET, Token::REC, rest..] => parse_let_rec(tokens),
        [Token::IF, rest..] => parse_if(tokens),
        [Token::FUNCTION, rest..] => parse_fun(tokens),
        /*[Token::LET, rest..] => parse_let(tokens),
        [Token::LBRACKET, Token::RBRACKET, rest..] => (Exp::Nil, rest),
        [Token::MATCH, rest..] => parse_match(tokens),*/
        _ => Err(format!("invalid token: {:?}",tokens)),
    }
}

fn parse_fun(tokens: &[Token]) -> Result<(Exp, &[Token]),String> {
    match tokens {
        [Token::FUNCTION, Token::VAR(s), Token::RARROW, rest..] => {
            let (exp, res) = parse_exp(rest)?;
            Ok((Exp::Func(s.clone(), box exp), res))
        }
        _ => Err(format!("{:?}", tokens)),
    }
}

fn parse_rec_fun(var: Var, tokens: &[Token]) -> Result<((Var,Var,Exp), &[Token]),String> {
    match tokens {
        [Token::FUNCTION, Token::VAR(s), Token::RARROW, rest..] => {
            let (exp, res) = parse_exp(rest)?;
            Ok(((var.clone(), s.clone(), exp), res))
        }
        _ => Err(format!("{:?}", tokens)),
    }
}

fn parse_let_rec(tokens: &[Token]) -> Result<(Exp, &[Token]),String> {
    match tokens {
        [Token::LET, Token::REC, Token::VAR(s), Token::EQUAL, rest..] => {
            let ((f_v,v,exp),res) = parse_rec_fun(s.clone(), rest)?;
            match res {
                [Token::IN, re..] => {
                    let (ex, r) = parse_exp(re)?;
                    Ok((Exp::LetRec(f_v, v,box exp,box ex),r))
                }
                _ => panic!("{:?}", res),
            }
        }
        _ => panic!(""),
    }
}

fn parse_app(exp: Exp, tokens: &[Token]) -> (Exp, &[Token]) {
    match parse_term(tokens) {
        Ok((ex,rest)) => parse_app(Exp::App(box exp, box ex), rest),
        _ => (exp,tokens)
    }
}

fn parse_if(tokens: &[Token]) -> Result<(Exp, &[Token]),String> {
    match tokens {
        [Token::IF, rest..] => {
            let (exp, res) = parse_exp(rest)?;
            match res {
                [Token::THEN, re..] => {
                    let (ex, r) = parse_exp(re)?;
                    match r {
                        [Token::ELSE, rr..] => {
                            let (e, rrr) = parse_exp(rr)?;
                            Ok((Exp::If(box exp, box ex, box e), rrr))
                        }
                        _ => Err(format!("{:?}", res)),
                    }
                }
                _ => Err(format!("{:?}", rest)),
            }
        }
        _ => Err(format!("{:?}", tokens)),
    }
}