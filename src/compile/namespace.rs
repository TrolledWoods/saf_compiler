use crate::tiny_string::TinyString;
use crate::parser::{ SourcePos, Identifier };
use crate::compile::CompileMemberId;
use std::collections::HashMap;
use std::sync::RwLock;

struct NamespaceMember {
    pos: SourcePos,
    data: CompileMemberId,
}

pub struct Namespaces {
    members: RwLock<HashMap<TinyString, NamespaceMember>>,
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
        member_id: CompileMemberId,
    ) -> Result<(), NamespaceError> {
        let mut members = self.members.write().unwrap();
        let old_member = members.insert(
            name.name, 
            NamespaceMember {
                pos: name.pos.clone(),
                data: member_id,
            }
        );

        // Is it ambiguous?
        if let Some(
            NamespaceMember { pos: old_pos, data: old_id, .. }
        ) = old_member {
            members.insert(
                name.name, 
                NamespaceMember {
                    pos: name.pos.clone(),
                    data: CompileMemberId::Poison,
                }
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
            println!("Namespace; '{}': {:?}", name.name, member_id);
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
            .map(|member| member.data)
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
