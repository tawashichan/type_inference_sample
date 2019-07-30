use std::collections::{HashSet,HashMap};

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
    TypeVar(TypeVarName),
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Bottom,
}

pub type Var = String;

pub type TypeVarName = String;

pub type TypeAssign = HashMap<TypeVarName,Type>;

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct TypeEquation(Type,Type);

pub type Restrictions = HashSet<TypeEquation>;

#[derive(Debug,Clone, PartialEq)]
pub enum TEnv {
    None,
    Env(Box<TEnv>,Var,Type),
}

impl TEnv {

    fn search(&self, var: &Var) -> Option<Type> {
        match self {
            TEnv::None => None,
            TEnv::Env(box prev, v,typ) => {
                if var == v {
                    Some(typ.clone())
                } else {
                    prev.search(var)
                }
            }
        }
    }

}

impl Exp {

    pub fn type_inference(&self,env: &TEnv) -> Type {
        let (res,typ) = self.extract(env);
        let type_assign = unify(res);
        typ.assign(&type_assign)
    }

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

                let e3_r = merge_restrictions(e1_r,e2_r);
                let op_equation = new_restrictions(vec![TypeEquation(e1_typ,Type::Int),TypeEquation(e2_typ,Type::Int)]);
                let e3_r = merge_restrictions(e3_r,op_equation);
               
                match op {
                    Op::ADD => (e3_r,Type::Int),
                    _ => (e3_r,Type::Bool)
                }
            }
            Exp::If(box cond,box then,box els) => {
                let (cond_r,cond_typ) = cond.extract(env);
                let (then_r,then_typ) = then.extract(env);
                let (els_r,els_typ) = els.extract(env);
                let r1 = merge_restrictions(cond_r,then_r);
                let r2 = merge_restrictions(els_r,new_restrictions(vec![TypeEquation(cond_typ,Type::Bool),TypeEquation(then_typ.clone(),els_typ)]));
                let r4 = merge_restrictions(r1, r2);
                (r4,then_typ)
            },
            Exp::App(box e1,box e2) => {
                let new_type_var = Type::TypeVar("aaa".to_string());
                let (e1_r,e1_typ) = e1.extract(env);
                let (e2_r,e2_typ) = e2.extract(env);
                let r1 = merge_restrictions(e1_r, e2_r);
                let r2 = merge_restrictions(r1,new_restrictions(vec![TypeEquation(e1_typ,Type::Func(box e2_typ,box new_type_var.clone()))])); 
                (r2,new_type_var)
            },
            Exp::Func(x,box exp) => {
                // globalにuniqueなtypeVarを出力する実装を考える!!
                let new_type_var = Type::TypeVar(x.to_string());
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

impl Type {
    fn assign(&self,assign: &TypeAssign) -> Type {
        match self {
            Type::Func(box Type::TypeVar(v),box typ) => {
                match assign.get(v) {
                    Some(t) => Type::Func(box t.clone(),box typ.assign(assign)),
                    _ => panic!()
                }
            }   
            _ => self.clone()
        }
    } 
}

fn new_restrictions(type_equations: Vec<TypeEquation>) -> Restrictions {
    type_equations.into_iter().collect()
} 

fn merge_restrictions(r1: Restrictions,r2: Restrictions) -> Restrictions {
    r1.into_iter().chain(r2).collect()
}

fn assign(typ_var_name: &TypeVarName,typ: &Type,res: &[TypeEquation]) -> Vec<TypeEquation> {
    res.iter().map(|a| 
        match a {
            TypeEquation(Type::TypeVar(v_n),t) if typ_var_name == v_n => {
                TypeEquation(typ.clone(),t.clone())
            }
            TypeEquation(t,Type::TypeVar(v_n)) if typ_var_name == v_n => {
                TypeEquation(typ.clone(),t.clone())
            }
            _ => a.clone()
        }
    ).collect()
}

pub fn unify(res: Restrictions) -> TypeAssign {
    let res: Vec<TypeEquation> = res.into_iter().collect();
    unify_sub(&res)
}

fn unify_sub(res: &[TypeEquation]) -> TypeAssign {
    match res {
        [] => TypeAssign::new(),
        [TypeEquation(t1,t2),rest..] if t1 == t2 => unify_sub(rest),
        [TypeEquation(Type::Func(box f1_a,box f1_b),Type::Func(box f2_a,box f2_b)),rest..] => {
            let res: Vec<TypeEquation> = res.iter().cloned().chain(vec![TypeEquation(f1_a.clone(),f2_a.clone()),TypeEquation(f1_b.clone(),f2_b.clone())]).collect();
            unify_sub(&res)
        },
        [TypeEquation(Type::TypeVar(v),typ),rest..] | [TypeEquation(typ,Type::TypeVar(v)),rest..] => {
            let mut s = unify_sub(&assign(v,typ,rest));
            s.insert(v.clone(),typ.clone());
            s
        }
        _ => panic!("{:?}",res)
    }
}
