//! # Singular branch matching
//!
//! This module contains code to build impl sections based on the first match in the rule. If this
//! first group meets some requirements, it can be built into a `match` fn at the start of the impl
//! and remove the need to clone the source iterator if the rule is not matched.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Pat;
use crate::build::save::value_save_other_get_args;
use crate::prelude::{Group, RuleInner, RuleInnerMatch, SaveType, SplitRule, Value};

pub trait IsSingular {
	fn is_singular(&self) -> bool;
}

impl IsSingular for Value {
	fn is_singular(&self) -> bool {
		match self {
			Value::Single(_) => true,
			Value::Save { group: SaveType::Other { .. }, .. } => true,
			Value::Group(g, _) => g.is_singular(),
			_ => false,
		}
	}
}

impl IsSingular for Group {
	fn is_singular(&self) -> bool {
		match self {
			Group::Literal(inner, _) => inner.is_singular(),
			_ => false
		}
	}
}

impl IsSingular for SplitRule {
	fn is_singular(&self) -> bool {
		match self {
			SplitRule::Single(v) => v.is_singular(),
			SplitRule::Other { start, .. } => start.is_singular(),
		}
	}
}

impl RuleInner {
	pub fn is_singular(&self) -> Option<(&Self, Pat, bool)> {
		let group = match &self.inner {
			RuleInnerMatch::Named(_, g) => g,
			RuleInnerMatch::Unnamed(g) => g
		};
		let first = match group {
			SplitRule::Single(g) => g,
			SplitRule::Other { .. } => return None,
		};
		if let Group::Literal(ref v, _) = **first {
			match v {
				Value::Single(e) => {
					Some((self, Pat::Verbatim(e.to_token_stream()), false))
				}
				Value::Save { group: SaveType::Other { pattern, .. }, .. } => {
					Some((self, pattern.clone(), true))
				}
				_ => None
			}
		} else { None }
	}
	
	pub fn match_arm(&self, p: Pat, wrapped: bool, saves: bool) -> TokenStream {
		let args = value_save_other_get_args(&p);
		let var = &self.name;
		match &self.inner {
			RuleInnerMatch::Named(names, g) => {
				match g {
					SplitRule::Single(_) => {
						if args.is_empty() {
							match (wrapped, saves) {
								(true, true) => quote!{ __next_unwrapped @ #p => Ok(cflp::NodeWrapper { start, end, node: #var(__next_unwrapped.clone()) }) },
								(true, false) => quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var }) },
								(false, true) => quote!{ __next_unwrapped @ #p => Ok(#var(__next_unwrapped.clone())) },
								(false, false) => quote!{ #p => Ok(#var) }
							}
						} else {
							let field_args = names.iter().zip(args.iter()).map(|(f, v)| quote!{ #f: #v.clone() });
							if wrapped {
								quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var{ #(#field_args),* } }) }
							} else {
								quote!{ #p => Ok(#var{ #(#field_args),* }) }
							}
						}
					}
					SplitRule::Other { .. } => unreachable!("I don't think this is reachable?\n{}", std::backtrace::Backtrace::force_capture())
				}
			}
			RuleInnerMatch::Unnamed(g) => {
				match g {
					SplitRule::Single(_) => {
						if args.is_empty() {
							match (wrapped, saves) {
								(true, true) => quote!{ __next_unwrapped @ #p => Ok(cflp::NodeWrapper { start, end, node: #var(__next_unwrapped.clone()) }) },
								(true, false) => quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var }) },
								(false, true) => quote!{ __next_unwrapped @ #p => Ok(#var(__next_unwrapped.clone())) },
								(false, false) => quote!{ #p => Ok(#var) }
							}
						} else {
							if wrapped {
								quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var(#(#args.clone()),*) }) }
							} else {
								quote!{ #p => Ok(#var(#(#args.clone()),*)) }
							}
						}
					}
					SplitRule::Other { .. } => {
						todo!()
					}
				}
			}
		}
	}
}
