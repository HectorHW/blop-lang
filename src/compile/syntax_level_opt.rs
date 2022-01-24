use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::{Token, TokenKind};
use std::collections::HashSet;

struct Optimizer {
    names: Vec<HashSet<String>>,
    total_variables: usize,
}

pub fn optimize(program: Program) -> Program {
    let mut optimizer = Optimizer::new();
    optimizer.new_scope();
    optimizer.visit_expr(program)
}

impl Optimizer {
    fn new() -> Optimizer {
        Optimizer {
            names: vec![],
            total_variables: 0,
        }
    }

    fn lookup_local(&mut self, _name: &Token) {}

    fn new_variable_slot(&mut self, variable_name: &Token) {
        self.total_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.get_string().unwrap().to_string());
    }

    fn new_scope(&mut self) {
        self.names.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.names.pop().unwrap();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::Print(t, e) => {
                let containing = self.visit_expr(e);
                Stmt::Print(t, containing)
            }

            Stmt::VarDeclaration(name, body) => {
                let body = body.map(|e| self.visit_expr(e));
                self.new_variable_slot(&name);
                Stmt::VarDeclaration(name, body)
            }

            Stmt::Assignment(target, expr) => {
                let body = self.visit_expr(expr);
                Stmt::Assignment(target, body)
            }
            Stmt::Expression(e) => match e {
                Expr::SingleStatement(s) => self.visit_stmt(*s),
                any_other => Stmt::Expression(self.visit_expr(any_other)),
            },
            a @ Stmt::Assert(..) => a, //never optimize assert expression
            Stmt::FunctionDeclaration { name, args, body } => {
                let function = self.check_function(name.clone(), args, body);
                self.new_variable_slot(&name);
                function
            }

            p @ Stmt::Pass(..) => p,
        }
    }

    fn visit_expr(&mut self, expr: Expr) -> Expr {
        let expr: Expr = match expr {
            n @ Expr::Number(..) => n,
            s @ Expr::ConstString(..) => s,

            na @ Expr::Name(..) => na,

            Expr::Unary(op, a) => {
                let a = self.visit_expr(*a);
                Expr::Unary(op, Box::new(a))
            }

            Expr::Binary(op, a, b) => {
                let a = self.visit_expr(*a);
                let b = self.visit_expr(*b);
                match (&a, &b) {
                    (Expr::Number(token_a), Expr::Number(token_b)) => {
                        let na = token_a.get_number().unwrap();
                        let nb = token_b.get_number().unwrap();
                        match op.kind {
                            TokenKind::Star => Expr::Number(Token {
                                position: token_a.position,
                                kind: TokenKind::Number(na * nb),
                            }),
                            TokenKind::Slash => match na.checked_div(nb) {
                                Some(result) => Expr::Number(Token {
                                    position: token_a.position,
                                    kind: TokenKind::Number(result),
                                }),
                                None => {
                                    eprintln!("encountered zero division while folding constants, assuming it is intended [{}]", op.position);
                                    Expr::Binary(op, Box::new(a), Box::new(b))
                                }
                            },

                            TokenKind::Mod => match na.checked_rem(nb) {
                                Some(result) => Expr::Number(Token {
                                    position: token_a.position,
                                    kind: TokenKind::Number(result),
                                }),
                                None => {
                                    eprintln!("encountered modulo 0 while folding constants, assuming it is intended [{}]", op.position);
                                    Expr::Binary(op, Box::new(a), Box::new(b))
                                }
                            },

                            TokenKind::Plus => Expr::Number(Token {
                                position: token_a.position,
                                kind: TokenKind::Number(na + nb),
                            }),
                            TokenKind::Minus => Expr::Number(Token {
                                position: token_a.position,
                                kind: TokenKind::Number(na - nb),
                            }),
                            TokenKind::CompareEquals => Expr::Number(Token {
                                position: token_a.position,
                                kind: TokenKind::Number(if na == nb { 1 } else { 0 }),
                            }),
                            TokenKind::Power => Expr::Number(Token {
                                position: token_a.position,
                                kind: TokenKind::Number(na.pow(nb as u64 as u32)),
                            }),
                            _any_other => panic!("unexpected binary operator {}", _any_other),
                        }
                    }
                    _other_cases => Expr::Binary(op, Box::new(a), Box::new(b)),
                }
            }

            Expr::If(cond, then_body, else_body) => {
                let cond = self.visit_expr(*cond);
                let then_body = self.visit_expr(*then_body);
                let else_body = else_body.map(|x| self.visit_expr(*x));
                Expr::If(
                    Box::new(cond),
                    Box::new(then_body),
                    else_body.map(|e| Box::new(e)),
                )
            }

            Expr::Block(bb, be, mut statements) => {
                if statements.len() == 1 {
                    let statement = statements.remove(0);
                    let e = Expr::SingleStatement(Box::new(statement));
                    self.visit_expr(e)
                } else {
                    let statements = statements.into_iter().map(|s| self.visit_stmt(s)).collect();
                    Expr::Block(bb, be, statements)
                }
            }
            Expr::Call(target, args) => {
                let target = self.visit_expr(*target);
                let args = args.into_iter().map(|a| self.visit_expr(a)).collect();
                Expr::Call(Box::new(target), args)
            }

            Expr::PartialCall(target, args) => {
                let target = self.visit_expr(*target);
                let args = args
                    .into_iter()
                    .map(|a| a.map(|a| self.visit_expr(a)))
                    .collect();
                Expr::PartialCall(Box::new(target), args)
            }

            Expr::SingleStatement(s) => match *s {
                //singleStatement is artificial node representing block with single statement
                Stmt::Print(t, p) => Expr::SingleStatement(Box::new(Stmt::Print(t, p))),

                Stmt::VarDeclaration(_name, maybe_body) => {
                    /*
                    thing like

                    if ...
                        var a = 2
                    ...

                    variable is not used anywhere else, can be substituted with expr
                    */
                    maybe_body.unwrap_or(Expr::Number(Token {
                        position: _name.position,
                        kind: TokenKind::Number(0),
                    }))
                }

                a @ Stmt::Assignment(..) => Expr::SingleStatement(Box::new(a)),

                Stmt::Expression(e) => self.visit_expr(e),

                a @ Stmt::Assert(..) => Expr::SingleStatement(Box::new(a)),

                Stmt::FunctionDeclaration { name, .. } => {
                    /*
                    thing like
                    if ...
                        def ... =
                            ...
                    ...

                    as def value is not used, it can be exluded from resulting program
                    */
                    Expr::Number(Token {
                        position: name.position,
                        kind: TokenKind::Number(0),
                    })
                    //Expr::SingleStatement(a)
                }

                p @ Stmt::Pass(..) => Expr::SingleStatement(Box::new(p)),
            },

            anon @ Expr::AnonFunction(..) => anon,
        };
        expr
    }

    fn check_function(&mut self, name: Token, args: Vec<Token>, body: Expr) -> Stmt {
        let mut scope_stack = vec![];
        std::mem::swap(&mut self.names, &mut scope_stack);
        let previous_total_variables = self.total_variables;
        self.total_variables = 0;

        self.new_scope();

        self.new_variable_slot(&name); //define function inside itself
        for arg_name in &args {
            self.new_variable_slot(arg_name);
        }
        let body = self.visit_expr(body);

        std::mem::swap(&mut self.names, &mut scope_stack);
        self.total_variables = previous_total_variables;

        Stmt::FunctionDeclaration { name, args, body }
    }

    fn visit_block(&mut self, block: Vec<Stmt>) -> Vec<Stmt> {
        self.new_scope();

        let mut res = vec![];

        for item in block {
            res.push(self.visit_stmt(item));
        }
        self.pop_scope();
        res
    }
}
