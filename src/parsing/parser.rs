use crate::parsing::ast::{Expr, Stmt};
use crate::parsing::lexer::{Token, TokenKind};

macro_rules! t {
    ($e:pat) => {
        Token { kind: $e, .. }
    };
}

macro_rules! bin {
    ($op:expr, $a:expr, $b:expr) => {
        Expr::Binary($op, Box::new($a), Box::new($b))
    };
}

enum CallVariant {
    Normal(Vec<Expr>),
    Partial(Vec<Option<Expr>>),
}

peg::parser! {
    pub grammar program_parser() for [Token] {
        use TokenKind::*;
        pub rule program() -> Expr
            = block_expr()

        rule block() -> (Token, Token, Vec<Stmt>) =
            [bb@t!(BeginBlock)] [t!(LineEnd)]? s:stmt() ** [t!(LineEnd)] [t!(LineEnd)]? [be@t!(EndBlock)] {(bb, be, s)}


        rule stmt() -> Stmt =

             var_decl_stmt()
            / function_decl_stmt()
            / print_stmt()
            / assignment_stmt()
            / assert_stmt()
            / pass_stmt()
            / e:expr() {Stmt::Expression(e)}


        rule var_decl_stmt() -> Stmt =
            [t!(Var)] n:name() e:assignment_right_side()?
                {Stmt::VarDeclaration(n, e)}

        rule assignment_right_side() -> Expr =
            [t!(Equals)] e:expr() {e}

        rule function_decl_stmt() -> Stmt =
            [t!(Def)] n:name() args:maybe_arguments_and_equals() body:expr() {
                Stmt::FunctionDeclaration{name:n, args, body}
            }

        rule paren_name_list() -> Vec<Token> =
            [t!(LParen)] n:name()**[t!(Comma)] [t!(Comma)]? [t!(RParen)] {n}

        rule maybe_arguments_and_equals() -> Vec<Token> =
            n:paren_name_list() [t!(Equals)] {
                n
            }
            / [t!(Equals)] {Vec::new()}

        rule print_stmt() -> Stmt =
            [print_token@t!(Print)] e:expr() {Stmt::Print(print_token, e)}

        rule assignment_stmt() -> Stmt =
            n:name() [t!(Equals)] e:expr() {Stmt::Assignment(n, e)}

        rule assert_stmt() -> Stmt =
            [a@t!(Assert)] e:expr() {Stmt::Assert(a, e)}

        rule if_expr() -> Expr =
            if_elif_else()/ if_elif() / if_then()

        rule if_elif() -> Expr =
            [t!(If)] cond:simple_expr() then:expr() [t!(LineEnd)]? elif:elif_body()+
                {
                    let mut last_if_cond = None;
                    for (cond, body) in elif.into_iter().rev() {
                        last_if_cond = Some(Box::new(Expr::IfExpr(Box::new(cond), Box::new(body), last_if_cond)));
                    }
                    Expr::IfExpr(Box::new(cond), Box::new(then), last_if_cond)
                }

        rule if_then() -> Expr =
            [t!(If)] cond:simple_expr() then:expr()
                {Expr::IfExpr(Box::new(cond), Box::new(then), None)}

        rule if_elif_else() -> Expr =
            [t!(If)] cond:simple_expr() then:expr() [t!(LineEnd)]? elif:elif_body()* [t!(Else)] else_body:expr()
                {
                    let mut last_if_cond = Some(Box::new(else_body));
                    for (cond, body) in elif.into_iter().rev() {
                        last_if_cond = Some(Box::new(Expr::IfExpr(Box::new(cond), Box::new(body), last_if_cond)));
                    }

                    Expr::IfExpr(Box::new(cond), Box::new(then), last_if_cond)}

        rule elif_body() -> (Expr, Expr) =
            [t!(ELif)] elif_cond:simple_expr() elif_body: expr() [t!(LineEnd)]? {
                (elif_cond, elif_body)
            }

        rule pass_stmt() -> Stmt =
            [t@t!(Pass)] {Stmt::Pass(t)}

        rule expr() -> Expr =
            block_expr() /
            if_expr() /
            simple_expr()

        rule block_expr() -> Expr =
            b:block() {Expr::Block(b.0, b.1, b.2)}

        rule simple_expr() -> Expr =
            arrow() /
            arithmetic()

        rule arrow() -> Expr =
            p:paren_name_list() [t@t!(Arrow)] b:simple_expr() {
            Expr::AnonFunction(p, t, Box::new(b))
        }

        rule arithmetic() -> Expr = precedence! {
            x: (@) [op@t!(Or)] y:@
                {bin!(op, x, y)}
            --
            x: (@) [op@t!(And)] y: @
                {bin!(op, x, y)}
            --
            [op@t!(Not)] x: @
                {
                    Expr::Unary(op, Box::new(x))
                }

            --
            x: (@) [op@t!(CompareEquals)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(CompareNotEquals)] y:@
                {bin!(op, x, y)}
            --
            x: (@) [op@t!(CompareGreater)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(CompareGreaterEqual)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(CompareLess)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(CompareLessEqual)] y:@
                {bin!(op, x, y)}

            --
            x: (@) [op@t!(Plus)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(Minus)] y:@
                {bin!(op, x, y)}
            --
            x: (@) [op@t!(Star)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(Slash)] y:@
                {bin!(op, x, y)}
            x: (@) [op@t!(Mod)] y:@
                {bin!(op, x, y)}
            --
            x:@ [op@t!(Power)] y:(@)
                {bin!(op, x, y)}
            --
            n:call() {n}
        }

        rule call() -> Expr =
            target:term() calls:call_parens()* {
                let mut res = target;
                for parens in calls {
                match parens {
                    CallVariant::Normal(args) => {
                        res = Expr::Call(Box::new(res), args)
                    }
                    CallVariant::Partial(args) => {
                        res = Expr::PartialCall(Box::new(res), args)
                    }
                }
            }
                res
            }
            / term()

        rule call_parens() -> CallVariant =
            [t!(LParen)] args:simple_expr()**[t!(Comma)] [t!(Comma)]? [t!(RParen)] {CallVariant::Normal(args)}
        / [t!(LParen)] args:maybe_argument()**[t!(Comma)] [t!(Comma)]? [t!(RParen)] {
            CallVariant::Partial(args)
        }

        rule maybe_argument() -> Option<Expr> =
            e:simple_expr() {Some(e)}
        / [t!(Blank)] {None}

        rule term() -> Expr
            = [num@t!(Number(..))] {Expr::Number(num)}
            / t:name()
                {Expr::Name(t)}
            / [s@t!(ConstString(..))] {Expr::ConstString(s)}
            / [t!(LParen)] e:expr() [t!(RParen)] {e}



        rule name() -> Token
            = [t@Token{kind:TokenKind::Name(..), position:pos}] {t}
    }
}
