use crate::parsing::ast::{Expr, Stmt};
use crate::parsing::lexer::{Token, TokenKind};

macro_rules! t {
    ($e:pat) => {
        Token { kind: $e, .. }
    };
}

peg::parser! {
    pub grammar program_parser() for [Token] {
        use TokenKind::*;
        pub rule program() -> Box<Expr>
            = block_expr()

        rule block() -> (Token, Vec<Stmt>) =
            [bb@t!(BeginBlock)] [t!(LineEnd)]? s:stmt() ** [t!(LineEnd)] [t!(LineEnd)]? [t!(EndBlock)] {(bb, s)}


        rule stmt() -> Stmt =

             var_decl_stmt()
            / function_decl_stmt()
            / print_stmt()
            / assignment_stmt()
            / assert_stmt()
            / e:expr() {Stmt::Expression(e)}


        rule var_decl_stmt() -> Stmt =
            [t!(Var)] n:name() e:assignment_right_side()?
                {Stmt::VarDeclaration(n, e)}

        rule assignment_right_side() -> Box<Expr> =
            [t!(Equals)] e:expr() {e}

        rule function_decl_stmt() -> Stmt =
            [t!(Def)] n:name() args:maybe_arguments_and_equals() body:expr() {
                Stmt::FunctionDeclaration{name:n, args, body}
            }

        rule maybe_arguments_and_equals() -> Vec<Token> =
            [t!(LParen)] n:name()**[t!(Comma)] [t!(Comma)]? [t!(RParen)] [t!(Equals)] {
                n
            }
            / [t!(Equals)] {Vec::new()}

        rule print_stmt() -> Stmt =
            [t!(Print)] e:expr() {Stmt::Print(e)}

        rule assignment_stmt() -> Stmt =
            n:name() [t!(Equals)] e:expr() {Stmt::Assignment(n, e)}

        rule assert_stmt() -> Stmt =
            [a@t!(Assert)] e:expr() {Stmt::Assert(a, e)}

        rule if_expr() -> Box<Expr> =
            if_elif_else()/ if_elif() / if_then()

        rule if_elif() -> Box<Expr> =
            [t!(If)] cond:simple_expr() then:expr() [t!(LineEnd)]? elif:elif_body()+
                {
                    let mut last_if_cond = None;
                    for (cond, body) in elif.into_iter().rev() {
                        last_if_cond = Some(Box::new(Expr::IfExpr(cond, body, last_if_cond)));
                    }
                    Box::new(Expr::IfExpr(cond, then, last_if_cond))
                }

        rule if_then() -> Box<Expr> =
            [t!(If)] cond:simple_expr() then:expr()
                {Box::new(Expr::IfExpr(cond, then, None))}

        rule if_elif_else() -> Box<Expr> =
            [t!(If)] cond:simple_expr() then:expr() [t!(LineEnd)]? elif:elif_body()* [t!(Else)] else_body:expr()
                {
                    let mut last_if_cond = Some(else_body);
                    for (cond, body) in elif.into_iter().rev() {
                        last_if_cond = Some(Box::new(Expr::IfExpr(cond, body, last_if_cond)));
                    }

                    Box::new(Expr::IfExpr(cond, then, last_if_cond))}

        rule elif_body() -> (Box<Expr>, Box<Expr>) =
            [t!(ELif)] elif_cond:simple_expr() elif_body: expr() [t!(LineEnd)]? {
                (elif_cond, elif_body)
            }


        rule expr() -> Box<Expr> =
            block_expr() /
            if_expr() /
            simple_expr()

        rule block_expr() -> Box<Expr> =
            b:block() {Box::new(Expr::Block(b.0, b.1))}

        rule simple_expr() -> Box<Expr> =
            arithmetic()

        rule arithmetic() -> Box<Expr> = precedence! {
            x: (@) [op@t!(TestEquals)] y:@
                {Box::new(Expr::Binary(op, x, y))}
            --
            x: (@) [op@t!(Plus)] y:@
                {Box::new(Expr::Binary(op, x, y))}
            x: (@) [op@t!(Minus)] y:@
                {Box::new(Expr::Binary(op, x, y))}
            --
            x: (@) [op@t!(Star)] y:@
                {Box::new(Expr::Binary(op, x, y))}
            x: (@) [op@t!(Slash)] y:@
                {Box::new(Expr::Binary(op, x, y))}
            --
            n:call() {n}
        }

        rule call() -> Box<Expr> =
            target:term() calls:call_parens()* {
                let mut res = target;
                for parens in calls {
                res = Box::new(Expr::Call(res, parens))
            }
                res
            }
            / term()

        rule call_parens() -> Vec<Box<Expr>> =
            [t!(LParen)] args:simple_expr()**[t!(Comma)] [t!(Comma)]? [t!(RParen)] {args}

        rule term() -> Box<Expr>
            = [t!(Number(x))] { Box::new(Expr::Number(x))}
            / t:name()
                {Box::new(Expr::Name(t))}
            / [t!(LParen)] e:expr() [t!(RParen)] {e}



        rule name() -> Token
            = [t@Token{kind:TokenKind::Name(..), position:pos}] {t}
    }
}
