use crate::ast::*;
use crate::types::*;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct StackFrame<'parent> {
    data: HashMap<String,Cell<DynamicType>>,
    parent: Option<&'parent StackFrame<'parent>>
} 

impl<'a> StackFrame<'a> {
    pub fn new() -> StackFrame<'static> {
        StackFrame {
            data: HashMap::new(),
            parent: None
        }
    }
    pub fn child(&'a self) -> StackFrame<'a> {
        StackFrame { data: HashMap::new(), parent: Some(self) }
    }
    pub fn resolve(&self, key: &str) -> Option<&Cell<DynamicType>> {
        let local = self.data.get(key);
        if local.is_some() {
            return local;
        }
        if let Some(parent) = self.parent {
            return parent.resolve(key);
        }
        None
    }
    pub fn get(&self, key: &str) -> Option<DynamicType> {
        match self.resolve(key) {
            Some(cell) => {
                let val = cell.take();
                let copy = val.clone();
                cell.set(val);
                Some(copy)
            },
            None => None
        }
    }
    pub fn set(&mut self, key: &str, val: DynamicType) -> Result<(),String> {
        if let Some(cell) = self.resolve(key) {
            cell.set(val);
            Ok(())
        } else {
            Err(format!("Tried to set variable {} but it doesn't exist!", key))
        }
    }
    pub fn push<S: Into<String>>(&mut self, key: S, val: DynamicType) {
        self.data.insert(key.into(), Cell::new(val));
    }
}

pub struct NativeFunctions {
    functions: HashMap<String,Box<dyn Fn(Vec<DynamicType>)->DynamicTypeResult>>,
} 

impl NativeFunctions {
    pub fn new() -> NativeFunctions {
        NativeFunctions {
            functions: HashMap::new()
        }
    }
    pub fn register_fn<S: Into<String>, F:'static + Fn(Vec<DynamicType>)->DynamicTypeResult>(&mut self, name: S, callable: F) {
        self.functions.insert(name.into(), Box::new(callable));
    }
    pub fn register_builtins(&mut self) {
        self.register_fn("print",|args: Vec<DynamicType>|{
            for v in args {
                print!("{}",v);
            }
            println!("");
            Ok(DynamicType::Void)
        });
    }
    pub fn get<T: Into<String>>(&self, func_name: T) -> Option<&Box<dyn Fn(Vec<DynamicType>)->DynamicTypeResult>> {
        self.functions.get(&func_name.into())
    }
}

pub type DefinedFunctions<'a> = RefCell<HashMap<String,FnDeclaration<'a>>>;

pub fn eval_scope<'source,'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, scope: &Scope<'source>) -> DynamicTypeResult {
    for decl in &scope.data {
        frame.push(decl.variable.name.text,DynamicType::Void);
    }

    for decl in &scope.data {
        if let Some(expr) = &decl.value {
            let new_val = eval_expr(frame, native_funcs, defined_funcs, &*expr)?;
            frame.set(decl.variable.name.text, new_val);
        }
    }
    let mut result = DynamicType::Void;
    for expr in &scope.statements {
        result = eval_expr(frame, native_funcs, defined_funcs, &expr)?;
    }
    Ok(result)
}

fn eval_expr<'source, 'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, expr: &Expression<'source>) -> DynamicTypeResult {
    match expr {
        Expression::Scope(scope) => eval_scope(frame, native_funcs, defined_funcs, scope),
        Expression::InfixOperation(infix) => eval_infix(frame, native_funcs, defined_funcs, infix),
        Expression::Literal(token) => DynamicType::from_token(token),
        Expression::Variable(var) => match frame.get(var.name.text) {
            Some(value) => Ok(value),
            None => Err(format!("Attempt to read variable {:?} but it doesn't exist!",var))
        },
        Expression::Callable(call) => {
            if let Some(result) = eval_builtins(frame, native_funcs, defined_funcs, call) {
                return result;
            }
            if let Expression::Variable(ref target) = &*call.name {
                let args = call.args.iter().map(|x|eval_expr(frame, native_funcs, defined_funcs, x).unwrap()).collect();
                if let Some(result) = eval_fn(frame, native_funcs, defined_funcs, call, &args) {
                    return result;
                }
                //println!("trying to call {} with {:?}",target.name.text,args);
                if let Some(func) = native_funcs.get(target.name.text) {
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
            defined_funcs.borrow_mut().insert(func.name.text.to_string(), func.clone());
            Ok(DynamicType::Void)
        },
        Expression::If(if_expr) => {
            let cond_result = eval_expr(frame, native_funcs, defined_funcs, &if_expr.condition)?;
            if cond_result == true {
                eval_scope(&mut frame.child(), native_funcs, defined_funcs, &if_expr.body)
            } else {
                if if_expr.else_body.is_some() {
                    eval_scope(&mut frame.child(), native_funcs, defined_funcs, if_expr.else_body.as_ref().unwrap())
                } else {
                    Ok(DynamicType::Void)
                }
            }
        },
        Expression::While(while_expr) => {
            while eval_expr(frame, native_funcs, defined_funcs, &while_expr.condition)? == true {
                eval_scope(&mut frame.child(), native_funcs, defined_funcs, &while_expr.body)?;
            }
            Ok(DynamicType::Void)
        }
        _ => panic!("Not implemented {:?}", expr)
    }
}

fn eval_infix<'source, 'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, infix: &InfixOperation<'source>) -> DynamicTypeResult {
    let lhs = eval_expr(frame, native_funcs, defined_funcs, &*infix.lhs)?;
    let rhs = eval_expr(frame, native_funcs, defined_funcs, &*infix.rhs)?;
    let res = match infix.operator.text {
        "+" => lhs + rhs,
        "-" => lhs - rhs,
        "*" => lhs * rhs,
        "/" => lhs / rhs,
        ">" => Ok(DynamicType::Bool(lhs > rhs)),
        "<" => Ok(DynamicType::Bool(lhs < rhs)),
        ">=" => Ok(DynamicType::Bool(lhs >= rhs)),
        "<=" => Ok(DynamicType::Bool(lhs <= rhs)),
        "==" => Ok(DynamicType::Bool(lhs == rhs)),
        "!=" => Ok(DynamicType::Bool(lhs != rhs)),
        "=" => {
            match &*infix.lhs {
                Expression::Variable(ref var) => {
                    frame.set(var.name.text, rhs)?;
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

pub fn invoke_fn<'source, 'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, function: &str, args:  &Vec<DynamicType>) -> Option<DynamicTypeResult> {
    let func = if let Some(func) = defined_funcs.borrow().get(function) {
        Some(func.clone())
    } else {
        None
    };
    if let Some(func) = func {
        if func.args.len() != args.len() {
            panic!("{:?} called with {} arguments, but it expects {}",func.name,args.len(),func.args.len());
        }
        let mut child_frame = frame.child();
        for arg in func.args.iter().zip(args.iter()) {
            child_frame.push(arg.0.name.text.to_string(),arg.1.clone());
        }
        Some(eval_scope(&mut child_frame,native_funcs,defined_funcs,&func.body))
    } else {
        None
    }
}

fn eval_fn<'source, 'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, call: &Callable<'source>, args: &Vec<DynamicType>) -> Option<DynamicTypeResult> {
    if let Expression::Variable(ref target) = &*call.name {
        invoke_fn(frame, native_funcs, defined_funcs,target.name.text,args)
    } else {
        panic!("Expected an identifier, got {:?}",call.name)
    }
}

fn eval_builtins<'source, 'data>(frame: &'data mut StackFrame, native_funcs: &NativeFunctions, defined_funcs: &mut DefinedFunctions<'source>, call: &Callable<'source>) -> Option<DynamicTypeResult> {
    match &*call.name {
        Expression::Variable(ref target) => {
            match target.name.text {
                "dbg" => {
                    if call.args.len() != 1 {
                        Some(Err(format!("{:?}: dbg expects exactly one expression as its argument! Got {}", target.name.location, call.args.len())))
                    } else {
                        let result = eval_expr(frame, native_funcs, defined_funcs,&call.args[0]);
                        if let Ok(ref value) = result {
                            println!("{}: {} = {}",call.args[0].location(),call.args[0],value);
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
