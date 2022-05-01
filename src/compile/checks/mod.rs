mod constant_folding;
mod expression_lift;
mod name_definition_check;
pub mod tree_rewriter;
pub mod tree_visitor;
pub mod variable_annotation_generation;

use crate::compile::checks::constant_folding::Folder;
use crate::compile::checks::expression_lift::ExpressionLifter;
use crate::compile::checks::name_definition_check::NameRedefinitionChecker;
use crate::compile::checks::variable_annotation_generation::AnnotationGenerator;
use crate::parsing::ast::Program;
use crate::parsing::lexer::Token;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum VariableType {
    Normal,
    Boxed,
    Closed,
    Global,
}

#[derive(Clone, Debug, Default)]
pub struct Annotations {
    /// variables declared inside blocks with corresponding type (boxed or normal)
    block_symbol_table: HashMap<Token, IndexMap<String, VariableType>>,
    closed_names_table: HashMap<Token, IndexSet<Token>>,

    ///mapping of variable mention to place where it was declared
    variable_bindings: HashMap<Token, Token>,
}

impl Annotations {
    fn new() -> Self {
        Default::default()
    }

    pub fn get_or_create_block_scope(
        &mut self,
        block_id: &Token,
    ) -> &mut IndexMap<String, VariableType> {
        if !self.block_symbol_table.contains_key(block_id) {
            self.block_symbol_table
                .insert(block_id.clone(), Default::default());
        }
        self.block_symbol_table.get_mut(block_id).unwrap()
    }

    pub fn get_block_scope(&self, block_id: &Token) -> Option<&IndexMap<String, VariableType>> {
        self.block_symbol_table.get(block_id)
    }

    pub fn get_or_create_closure_scope(&mut self, closure_id: &Token) -> &mut IndexSet<Token> {
        if !self.closed_names_table.contains_key(closure_id) {
            self.closed_names_table
                .insert(closure_id.clone(), Default::default());
        }
        self.closed_names_table.get_mut(closure_id).unwrap()
    }

    pub fn get_closure_scope(&self, closure_id: &Token) -> Option<&IndexSet<Token>> {
        self.closed_names_table.get(closure_id)
    }
}

pub fn check_optimize(tree: Program) -> Result<(Program, Annotations), String> {
    NameRedefinitionChecker::check(&tree)?;
    let tree = ExpressionLifter::optimize(tree)?;
    let mut annotations = Annotations::new();
    AnnotationGenerator::generate_annotations(&tree, &mut annotations)?;
    let tree = Folder::fold_constants(tree)?;

    Ok((tree, annotations))
}
