use crate::parsing::ast::Program;

use super::checks::Annotations;

pub mod typechecker;
pub mod types;

pub fn typecheck<'p, 'a>(
    program: &'p Program,
    annotations: &'a Annotations,
) -> Result<typechecker::Typemap<'p>, typechecker::TypeError> {
    typechecker::Checker::typecheck(program, annotations)
}
