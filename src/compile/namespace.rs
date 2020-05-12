use crate::tiny_string::TinyString;
use crate::parser::{ SourcePos, Identifier };
use crate::compile::CompileMemberId;
use std::collections::HashMap;
use std::sync::RwLock;

pub struct Namespaces {
    members: RwLock<HashMap<
        TinyString, 
        (SourcePos, CompileMemberId)
    >>,
}

impl Namespaces {
    pub fn new() -> Namespaces {
        Namespaces {
            members: RwLock::new(HashMap::new()),
        }
    }

    pub fn insert_member(
        &self,
        namespace_id: usize,
        name: Identifier,
        member: CompileMemberId,
    ) -> Result<(), NamespaceError> {
        let mut members = self.members.write().unwrap();
        let old_member = members.insert(
            name.name, 
            (name.pos.clone(), member),
        );

        // Is it ambiguous?
        if let Some((old_pos, old_id)) = old_member {
            members.insert(
                name.name, 
                (
                    name.pos.clone(), 
                    CompileMemberId::Poison
                ),
            );

            if let CompileMemberId::Poison = old_id {
                Err(NamespaceError::Poison)
            }else {
                Err(NamespaceError::NameClash {
                    namespace_id,
                    old: Identifier {
                        pos: old_pos,
                        name: name.name,
                    },
                    newly_added: name,
                })
            }
        }else {
            Ok(())
        }
    }

    pub fn find_value(
        &self, 
        namespace_id: usize, 
        name: TinyString,
    ) -> Option<CompileMemberId> {
        self.members
            .read()
            .unwrap()
            .get(&name)
            .map(|(_, member)| *member)
    }
}

#[derive(Debug)]
pub enum NamespaceError {
    Poison,
    NameClash {
        namespace_id: usize, 
        newly_added: Identifier,
        old: Identifier,
    },
    NotDefined,
}
