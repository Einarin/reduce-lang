use crate::ast::*;
use crate::types::*;
use std::collections::HashMap;

pub struct Context {
    pub data: HashMap<String,DynamicType>,
    pub functions: HashMap<String,Box<Fn(Vec<DynamicType>)->DynamicTypeResult>>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            data: HashMap::new(),
            functions: HashMap::new()
        }
    }
}

pub fn eval_scope(ctx: &mut Context, scope: &Scope) -> DynamicTypeResult {
    for decl in &scope.data {
        ctx.data.insert(decl.variable.name.text.to_string(),DynamicType::Void);
    }

    for decl in &scope.data {
        if let Some(expr) = &decl.value {
            let new_val = eval_expr(ctx, &*expr)?;
            ctx.data.insert(decl.variable.name.text.to_string(), new_val);
        }
    }
    let mut result = DynamicType::Void;
    for expr in &scope.statements {
        result = eval_expr(ctx, &expr)?;
    }
    Ok(result)
}

fn eval_expr(ctx: &mut Context, expr: &Expression) -> DynamicTypeResult {
    match expr {
        Expression::Scope(scope) => eval_scope(ctx, scope),
        Expression::InfixOperation(infix) => eval_infix(ctx, infix),
        Expression::Literal(token) => DynamicType::from_token(token),
        Expression::Variable(var) => Ok(ctx.data[var.name.text].clone()),
        Expression::Callable(call) => {
            if let Expression::Variable(ref target) = &*call.name {
                let args = call.args.iter().map(|x|eval_expr(ctx, x).unwrap()).collect();
                //println!("trying to call {} with {:?}",target.name.text,args);
                ctx.functions[target.name.text](args)
            } else {
                panic!("Expected an identifier, got {:?}",call.name)
            }
            
        },
        Expression::Import(_) => Ok(DynamicType::Void),
        Expression::Declaration(_) => Ok(DynamicType::Void),
        _ => panic!("Not implemented {:?}", expr)
    }
}

fn eval_infix(ctx: &mut Context, infix: &InfixOperation) -> DynamicTypeResult {
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
                    ctx.data.insert(var.name.text.to_string(), new_val);
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