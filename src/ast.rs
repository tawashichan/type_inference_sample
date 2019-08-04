use std::collections::{HashSet,HashMap};
use nanoid;

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Var(Var),
    Int(i64),
    Bool(bool),
    Op(Op, Box<Exp>, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Func(Var, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Let(Let),
    LetRec(Var,Var,Box<Exp>,Box<Exp>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Lt,
    Equal
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
            Exp::Bool(b) => (Restrictions::new(),Type::Bool),
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
                    Op::Plus => (e3_r,Type::Int),
                    Op::Minus => (e3_r,Type::Int),
                    _ => (e3_r,Type::Bool)
                }
            }
            Exp::Let(Let(v,box e1,box e2)) => {
                let (e1_r,e1_typ) = e1.extract(env);
                let new_env = TEnv::Env(
                    box env.clone(),
                    v.clone(),
                    e1_typ,
                );
                let (e2_r,e2_typ) = e2.extract(&new_env);
                let r = merge_restrictions(e1_r,e2_r);
                (r,e2_typ)
            }
            Exp::LetRec(x,y,box e1,box e2) => {
                let type_var_x = Type::TypeVar(gen_unique_type_var_name());
                let type_var_y = Type::TypeVar(gen_unique_type_var_name());
                let x_env = TEnv::Env(
                    box env.clone(),
                    x.clone(),
                    type_var_x.clone()
                );
                let y_env = TEnv::Env(
                    box x_env.clone(),
                    y.clone(),
                    type_var_y.clone(),
                );
                let (e1_r,e1_typ) = e1.extract(&y_env);
                let (e2_r,e2_typ) = e2.extract(&x_env);
                let r = merge_restrictions(
                    merge_restrictions(e1_r,e2_r), 
                    new_restrictions(vec![TypeEquation(type_var_x,Type::Func(box type_var_y,box e1_typ))])
                );
                (r,e2_typ)
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
                let new_type_var = Type::TypeVar(gen_unique_type_var_name());
                let (e1_r,e1_typ) = e1.extract(env);
                let (e2_r,e2_typ) = e2.extract(env);
                let r1 = merge_restrictions(e1_r, e2_r);
                let r2 = merge_restrictions(r1,new_restrictions(vec![TypeEquation(e1_typ,Type::Func(box e2_typ,box new_type_var.clone()))])); 
                (r2,new_type_var)
            },
            Exp::Func(x,box exp) => {
                // globalにuniqueなtypeVarを出力する実装を考える!!
                let new_type_var = Type::TypeVar(gen_unique_type_var_name());
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
                    Some(t) => Type::Func(box t.assign(assign),box typ.assign(assign)),
                    _ => panic!()
                }
            },
            Type::TypeVar(v) => {
                match assign.get(v) {
                    Some(t) => t.assign(assign),
                    _ => panic!("{:?} {:?}",v,assign)
                }
            },
            _ => self.clone()
        }
    } 

    fn assign_type_eq(&self,typ_var_name: &TypeVarName,typ: &Type) -> Type {
        match self {
            Type::Func(box t1,box t2) => {
                Type::Func(
                    box t1.assign_type_eq(typ_var_name,typ),
                    box t2.assign_type_eq(typ_var_name,typ)
                )
            },
            Type::TypeVar(v) if v == typ_var_name => {
                typ.clone()
            },
            _ => self.clone()
        }
    } 
}

fn gen_unique_type_var_name() -> TypeVarName {
    nanoid::simple()
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
            TypeEquation(t1,t2) => TypeEquation(t1.assign_type_eq(typ_var_name,typ),t2.assign_type_eq(typ_var_name,typ)),
            _ => a.clone()
        }
    ).collect()
}

pub fn unify(res: Restrictions) -> TypeAssign {
    let res: Vec<TypeEquation> = res.into_iter().collect();
    unify_sub(&res)
}

// ここでvecの処理が順序依存になっているので、行われるべき代入が行われないパターンが存在する
fn unify_sub(res: &[TypeEquation]) -> TypeAssign {
    match res {
        [] => TypeAssign::new(),
        [TypeEquation(t1,t2),rest..] if t1 == t2 => {
            unify_sub(rest)
        },
        [TypeEquation(Type::Func(box f1_a,box f1_b),Type::Func(box f2_a,box f2_b)),rest..] => {
            let re: Vec<TypeEquation> = rest.iter().cloned().chain(vec![TypeEquation(f1_a.clone(),f2_a.clone()),TypeEquation(f1_b.clone(),f2_b.clone())]).collect();
            unify_sub(&re)
        },
        [TypeEquation(Type::TypeVar(v),typ),rest..] | [TypeEquation(typ,Type::TypeVar(v)),rest..] => {
            let mut s = unify_sub(&assign(v,typ,rest));
            s.insert(v.clone(),typ.clone());
            s
        }
        _ => panic!("{:?}",res)
    }
}
