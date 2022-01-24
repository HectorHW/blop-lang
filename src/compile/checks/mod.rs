mod constant_folding;
mod name_definition_check;
mod tree_visitor;

use crate::compile::checks::constant_folding::Folder;
use crate::compile::checks::name_definition_check::NameRedefinitionChecker;
use crate::parsing::ast::Program;

pub fn check_optimize(tree: Program) -> Result<Program, String> {
    let tree = NameRedefinitionChecker::check(tree)?;
    let tree = Folder::fold_constants(tree)?;
    Ok(tree)
}
