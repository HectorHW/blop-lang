mod constant_folding;
mod expression_lift;
mod name_definition_check;
mod tree_visitor;
mod variable_annotation_generation;

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
}

#[derive(Clone, Debug, Default)]
pub struct Annotations {
    /// variables declared inside blocks with corresponding type (boxed or normal)
    block_symbol_table: HashMap<Token, IndexMap<String, VariableType>>,
    closed_names_table: HashMap<Token, IndexSet<String>>,
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

    pub fn get_or_create_closure_scope(&mut self, closure_id: &Token) -> &mut IndexSet<String> {
        if !self.closed_names_table.contains_key(closure_id) {
            self.closed_names_table
                .insert(closure_id.clone(), Default::default());
        }
        self.closed_names_table.get_mut(closure_id).unwrap()
    }

    pub fn get_closure_scope(&self, closure_id: &Token) -> Option<&IndexSet<String>> {
        self.closed_names_table.get(closure_id)
    }
}

pub fn check_optimize(tree: Program) -> Result<(Program, Annotations), String> {
    let tree = NameRedefinitionChecker::check(tree)?;
    let tree = ExpressionLifter::optimize(tree)?;
    let mut annotations = Annotations::new();
    let tree = AnnotationGenerator::generate_annotations(tree, &mut annotations)?;
    let tree = Folder::fold_constants(tree)?;

    println!("blocks:");
    for (id, mapping) in &annotations.block_symbol_table {
        println!("{:?} - {:?}", id, mapping);
    }
    println!("closures:");
    for (id, closures) in &annotations.closed_names_table {
        println!("{:?} - {:?}", id, closures);
    }

    Ok((tree, annotations))
}
