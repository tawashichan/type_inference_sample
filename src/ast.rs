use std::collections::{HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Var(Var),
    Int(i64),
    Bool(bool),
    Op(Op, Box<Exp>, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Func(Var, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Let(Let)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    ADD,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let(Var, Box<Exp>, Box<Exp>);

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum Type {
    TypeVar(Var),
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Bottom,
}

pub type Var = String;

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct Assign(Var,Type);

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct TypeEquation(Type,Type);

pub type Restrictions = HashSet<TypeEquation>;

#[derive(Debug,Clone, PartialEq)]
pub enum TEnv {
    None,
    Env(Box<TEnv>, Var,Type),
}

impl TEnv {

    fn search(&self, var: &Var) -> Option<Type> {
        match self {
            TEnv::None => None,
            TEnv::Env(box prev, v,typ) => {
                if var == v {
                    Some(typ.clone())
                } else {
                    None
                }
            }
        }
    }

}

impl Exp {
    // パフォーマンスのことはなにも考えていない実装
    // ライフタイムパズルをしたいわけではないので、参照を使うのはあとまわし
    pub fn extract(&self,env: &TEnv) -> (Restrictions,Type) {
        match self {
            Exp::Int(i) => (Restrictions::new(),Type::Int),
            Exp::Var(v) => {
                let v_typ = env.search(v).unwrap();
                (Restrictions::new(),v_typ)
            }
            Exp::Op(op,box e1,box e2) => {
                let (e1_r,e1_typ) = e1.extract(env);
                let (e2_r,e2_typ) = e2.extract(env);

                let e3_r: Restrictions = e1_r.union(&e2_r).into_iter().map(|a| a.clone()).collect();
                let op_equation: Restrictions = vec![TypeEquation(e1_typ,Type::Int),TypeEquation(e2_typ,Type::Int)].into_iter().collect();
                let e3_r: Restrictions = e3_r.union(&op_equation).into_iter().map(|a| a.clone()).collect();
               
                match op {
                    Op::ADD => (e3_r,Type::Int),
                    _ => (e3_r,Type::Bool)
                }
            }
            Exp::Func(x,box exp) => {
                // globalにuniqueなtypeVarを出力する実装を考える!!
                let new_type_var = Type::TypeVar("1".to_string());
                let new_env = TEnv::Env(
                    box env.clone(),
                    x.clone(),
                    new_type_var.clone()
                );
                let (e_r,e_typ) = exp.extract(&new_env);
                (e_r,Type::Func(box new_type_var,box e_typ))
            }
            _ => (Restrictions::new(),Type::Bottom)
        }
    }
}

pub fn unify(res: Restrictions,typ: Type) {
    
}