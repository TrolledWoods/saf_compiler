use crate::tiny_string::TinyString;
use crate::parser::{ SourcePos, Identifier };
use crate::compile::CompileMemberId;
use std::collections::HashMap;
use std::sync::RwLock;
use std::mem::drop;

enum NamespaceMember {
    Defined {
        pos: SourcePos,
        data: CompileMemberId,
    },
    NotDefined {
        dependants: Vec<(SourcePos, CompileMemberId)>,
    },
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
    ) -> Result<
        Vec<(SourcePos, CompileMemberId)>, 
        NamespaceError
    > {
        let mut members = self.members.write().unwrap();
        let old_member = members.insert(
            name.name, 
            NamespaceMember::Defined {
                pos: name.pos.clone(),
                data: member_id,
            }
        );

        // Is it ambiguous?
        match old_member {
            Some(NamespaceMember::Defined { 
                pos: old_pos, 
                data: old_id, 
            }) => {
                members.insert(
                    name.name, 
                    NamespaceMember::Defined {
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
            }
            Some(NamespaceMember::NotDefined {
                dependants,
            }) => {
                println!(
                    "Namespace with deps: '{}': {:?}", 
                    name.name,
                    member_id
                );
                Ok(dependants)
            }
            None => {
                println!(
                    "Namespace: '{}': {:?}", 
                    name.name,
                    member_id
                );
                Ok(vec![])
            }
        }
    }

    /// Adds a dependency. This will make that dependant
    /// be returned to the caller of the ``insert_member``
    /// if that caller defined the thing you're depending on,
    /// or it will produce a compilation error if nobody
    /// ever set the value. That's why you have to pass
    /// a ``SourcePos``, because otherwise we have no way
    /// of getting a good error message.
    ///
    /// It may return a Some() if the value you're depending
    /// on is already defined.
    ///
    /// It takes a write lock to the members of the namespace,
    /// so make sure there is no read lock to them when
    /// calling this function.
    pub fn add_dependency(
        &self,
        namespace_id: usize,
        name: TinyString,
        dependant_pos: SourcePos,
        dependant: CompileMemberId,
    ) -> Option<CompileMemberId> {
        let mut members = self.members.write().unwrap();
        let member = members.get_mut(&name);
        match member {
            Some(NamespaceMember::Defined {
                data, ..
            }) => Some(*data),
            Some(NamespaceMember::NotDefined {
                dependants,
            }) => {
                dependants.push((dependant_pos, dependant));
                None
            }
            None => {
                members.insert(
                    name,
                    NamespaceMember::NotDefined {
                        dependants: 
                            vec![(dependant_pos, dependant)]
                    }
                );
                None
            }
        }
    }

    pub fn find_value(
        &self, 
        namespace_id: usize, 
        name: TinyString,
    ) -> Option<CompileMemberId> {
        match self.members
                .read()
                .unwrap()
                .get(&name) {
            Some(NamespaceMember::Defined {
                data, ..
            }) => Some(*data),
            _ => None
        }
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
