use crate::parsing::ast::{Expr, Op, Stmt};
use crate::parsing::lexer::{Token, TokenKind};

macro_rules! t {
    ($e:pat) => {
        Token { kind: $e, .. }
    };
}

peg::parser! {
    pub grammar program_parser() for [Token] {
        use TokenKind::*;
        pub rule program() -> Vec<Stmt>
            = block()

        rule block() -> Vec<Stmt> =
            [t!(BeginBlock)] s:stmt() ** [t!(LineEnd)] [t!(LineEnd)]? [t!(EndBlock)] {s}


        rule stmt() -> Stmt =

             var_decl_stmt()
            / print_stmt()
            / assignment_stmt()
            / e:expr() {Stmt::Expression(e)}


        rule var_decl_stmt() -> Stmt =
            [t!(Var)] n:name() e:assignment_left_side()?
                {Stmt::VarDeclaration(n, e)}

        rule assignment_left_side() -> Box<Expr> =
            [t!(Equals)] e:expr() {e}

        rule print_stmt() -> Stmt =
            [t!(Print)] e:expr() {Stmt::Print(e)}

        rule assignment_stmt() -> Stmt =
            n:name() [t!(Equals)] e:expr() {Stmt::Assignment(n, e)}

        rule if_expr() -> Box<Expr> =
            if_then_else() / if_then()

        rule if_then() -> Box<Expr> =
            [t!(If)] cond:simple_expr() then:expr()
                {Box::new(Expr::IfExpr(cond, then, None))}

        rule if_then_else() -> Box<Expr> =
            [t!(If)] cond:simple_expr() then:expr() [t!(LineEnd)] [t!(Else)] else_body:expr()
                {Box::new(Expr::IfExpr(cond, then, Some(else_body)))}

        rule expr() -> Box<Expr> =
            [t!(LineEnd)] b:block() {Box::new(Expr::Block(b))} /
            if_expr() /
            simple_expr()

        rule simple_expr() -> Box<Expr> =
            arithmetic()

        rule arithmetic() -> Box<Expr> = precedence! {
            x: (@) [t!(TestEquals)] y:@
                {Box::new(Expr::Binary(Op::TestEquals, x, y))}
            --
            x: (@) [t!(Plus)] y:@
                {Box::new(Expr::Binary(Op::Add, x, y))}
            x: (@) [t!(Minus)] y:@
                {Box::new(Expr::Binary(Op::Sub, x, y))}
            --
            x: (@) [t!(Star)] y:@
                {Box::new(Expr::Binary(Op::Mul, x, y))}
            x: (@) [t!(Slash)] y:@
                {Box::new(Expr::Binary(Op::Div, x, y))}
            --
            n:term() {n}
        }

        rule term() -> Box<Expr>
            = [t!(Number(x))] { Box::new(Expr::Number(x))}
            / t:name()
                {Box::new(Expr::Name(t))}
            / [t!(LParen)] e:expr() [t!(RParen)] {e}



        rule name() -> Token
            = [Token{kind:TokenKind::Name(s), position:pos}] {Token{kind:TokenKind::Name(s), position:pos}}
    }
}
