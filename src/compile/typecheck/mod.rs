use crate::parsing::ast::Program;

use super::checks::Annotations;

mod type_builder;
pub mod typechecker;
pub mod types;

pub fn typecheck<'p>(
    program: &'p Program,
    annotations: &'p Annotations,
) -> Result<typechecker::Typemap<'p>, typechecker::TypeError> {
    typechecker::Checker::typecheck(program, annotations)
}
