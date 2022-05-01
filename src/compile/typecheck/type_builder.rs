use crate::{compile::checks::Annotations, parsing::ast::TypeMention};

use super::{
    typechecker::{SomewhereTypeError, Typemap},
    types::Type,
};

pub struct TypeBuilder<'a, 'b, 'ast> {
    annotations: &'a Annotations,
    types: &'b Typemap<'ast>,
}

impl<'a, 'b, 'ast> TypeBuilder<'a, 'b, 'ast> {
    pub fn build_type(
        annotations: &'a Annotations,
        types: &'b Typemap<'ast>,
        t: &'ast TypeMention,
    ) -> Result<Type, SomewhereTypeError> {
        let builder = TypeBuilder { annotations, types };

        builder.visit_typemention(t)
    }

    fn visit_typemention(&self, t: &'ast TypeMention) -> Result<Type, SomewhereTypeError> {
        match t {
            TypeMention::Simple(s) => {
                if let Some(definition) = self.annotations.get_definiton(s) {
                    Ok(self.types.type_of(definition.into()))
                } else {
                    match s.get_string().unwrap() {
                        "Int" => Ok(Type::Int),
                        "Bool" => Ok(Type::Bool),
                        "Nothing" => Ok(Type::Nothing),
                        "Float" => Ok(Type::Float),
                        "Any" => Ok(Type::Unspecified),
                        _ => Err(SomewhereTypeError::UnknownType { value: t.clone() }),
                    }
                }
            }
            TypeMention::Function {
                kw: _,
                args,
                vararg,
                return_type,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.visit_typemention(t))
                    .collect::<Result<Vec<_>, SomewhereTypeError>>()?;
                let vararg = vararg
                    .as_ref()
                    .map(|v| v.as_ref())
                    .map(|t| self.visit_typemention(t))
                    .transpose()?;
                let ret = self.visit_typemention(return_type.as_ref())?;
                Ok(Type::build_function(args, vararg, ret))
            }
        }
    }
}
