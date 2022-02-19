use crate::ast::*;
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Context<'a> {
    pub data: Rc<RefCell<HashMap<String,DynamicType>>>,
    pub native_functions: HashMap<String,Rc<dyn Fn(Vec<DynamicType>)->DynamicTypeResult>>,
    pub functions: HashMap<String,FnDeclaration<'a>>,
}

impl<'a> Context<'a> {
    pub fn new(_source: &'a str) -> Context<'a> {
        Context {
            data: Rc::new(RefCell::new(HashMap::new())),
            native_functions: HashMap::new(),
            functions: HashMap::new()
        }
    }
    pub fn child(&self) -> Context<'a> {
        Context { data: self.data.clone(), native_functions: HashMap::new(), functions: HashMap::new() }
    }
    pub fn register_fn<S: Into<String>>(&mut self, name: S, callable: Rc<dyn Fn(Vec<DynamicType>)->DynamicTypeResult>) {
        self.native_functions.insert(name.into(), callable);
    }

    pub fn register_builtins(&mut self) {
        self.register_fn("print",Rc::new(|args: Vec<DynamicType>|{
            for v in args {
                print!("{}",v);
            }
            println!("");
            Ok(DynamicType::Void)
        }));
    }
}

pub fn eval_scope<'a>(ctx: &mut Context<'a>, scope: &Scope<'a>) -> DynamicTypeResult {
    for decl in &scope.data {
        ctx.data.borrow_mut().insert(decl.variable.name.text.to_string(),DynamicType::Void);
    }

    for decl in &scope.data {
        if let Some(expr) = &decl.value {
            let new_val = eval_expr(ctx, &*expr)?;
            ctx.data.borrow_mut().insert(decl.variable.name.text.to_string(), new_val);
        }
    }
    let mut result = DynamicType::Void;
    for expr in &scope.statements {
        result = eval_expr(ctx, &expr)?;
    }
    Ok(result)
}

fn eval_expr<'a>(ctx: &mut Context<'a>, expr: &Expression<'a>) -> DynamicTypeResult {
    match expr {
        Expression::Scope(scope) => eval_scope(ctx, scope),
        Expression::InfixOperation(infix) => eval_infix(ctx, infix),
        Expression::Literal(token) => DynamicType::from_token(token),
        Expression::Variable(var) => Ok(ctx.data.borrow()[var.name.text].clone()),
        Expression::Callable(call) => {
            if let Some(result) = eval_builtins(ctx,call) {
                return result;
            }
            if let Expression::Variable(ref target) = &*call.name {
                let args = call.args.iter().map(|x|eval_expr(ctx, x).unwrap()).collect();
                if let Some(result) = eval_fn(ctx,call,&args) {
                    return result;
                }
                //println!("trying to call {} with {:?}",target.name.text,args);
                if let Some(func) = ctx.native_functions.get(target.name.text) {
                    func(args)
                } else {
                    panic!("Tried to call {} at {:?}",target.name.text, target.name)
                }
            } else {
                panic!("Expected an identifier, got {:?}",call.name)
            }
            
        },
        Expression::Import(_) => Ok(DynamicType::Void),
        Expression::Declaration(_) => Ok(DynamicType::Void),
        Expression::FnDeclaration(func) => {
            ctx.functions.insert(func.name.text.to_string(), func.clone());
            Ok(DynamicType::Void)
        },
        _ => panic!("Not implemented {:?}", expr)
    }
}

fn eval_infix<'a>(ctx: &mut Context<'a>, infix: &InfixOperation<'a>) -> DynamicTypeResult {
    //println!("Evaluating {}",infix.operator.text);
    let res = match infix.operator.text {
        "+" => eval_expr(ctx, &*infix.lhs)? + eval_expr(ctx, &*infix.rhs)?,
        "-" => eval_expr(ctx, &*infix.lhs)? - eval_expr(ctx, &*infix.rhs)?,
        "*" => eval_expr(ctx, &*infix.lhs)? * eval_expr(ctx, &*infix.rhs)?,
        "/" => eval_expr(ctx, &*infix.lhs)? / eval_expr(ctx, &*infix.rhs)?,
        "=" => {
            match &*infix.lhs {
                Expression::Variable(ref var) => {
                    let new_val = eval_expr(ctx, &*infix.rhs)?;
                    ctx.data.borrow_mut().insert(var.name.text.to_string(), new_val);
                    Ok(DynamicType::Void)
                },
                Expression::Declaration(_) => Ok(DynamicType::Void),
                _ => panic!("Not implemented {:?}", infix)
            }
        },
        _ => panic!("Not implemented {:?}", infix.operator.text)
    };
    //println!("result {:?}", res);
    res
}

pub fn invoke_fn<'a>(ctx: &mut Context<'a>, function: &str, args:  &Vec<DynamicType>) -> Option<DynamicTypeResult> {
    if let Some(func) = ctx.functions.get(function) {
        if func.args.len() != args.len() {
            panic!("{:?} called with {} arguments, but it expects {}",func.name,args.len(),func.args.len());
        }
        let mut func_ctx = ctx.clone();
        for arg in func.args.iter().zip(args.iter()) {
            func_ctx.data.borrow_mut().insert(arg.0.name.text.to_string(),arg.1.clone());
        }
        Some(eval_scope(&mut func_ctx,&func.body))
    } else {
        None
    }
}

fn eval_fn<'a>(ctx: &mut Context<'a>, call: &Callable<'a>, args: &Vec<DynamicType>) -> Option<DynamicTypeResult> {
    if let Expression::Variable(ref target) = &*call.name {
        invoke_fn(ctx,target.name.text,args)
    } else {
        panic!("Expected an identifier, got {:?}",call.name)
    }
}

fn eval_builtins<'a>(ctx: &mut Context<'a>, call: &Callable<'a>) -> Option<DynamicTypeResult> {
    match &*call.name {
        Expression::Variable(ref target) => {
            match target.name.text {
                "dbg" => {
                    if call.args.len() != 1 {
                        Some(Err(format!("{:?}: dbg expects exactly one expression as its argument! Got {}", target.name.location, call.args.len())))
                    } else {
                        let result = eval_expr(ctx,&call.args[0]);
                        if let Ok(ref value) = result {
                            println!("{:?} = {}",call.args[0],value);
                        }
                        Some(result)
                    }
                },
                _ => None
            }
        },
        _ =>  None
    }
}
