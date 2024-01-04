//! # Singular branch matching
//!
//! This module contains code to build impl sections based on the first match in the rule. If this
//! first group meets some requirements, it can be built into a `match` fn at the start of the impl
//! and remove the need to clone the source iterator if the rule is not matched.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Expr, Pat};
use crate::build::save::value_save_other_get_args;
use crate::prelude::{Group, RuleInner, RuleInnerMatch, SaveType, SplitRule, Value};

impl RuleInner {
	pub fn is_singular(&self) -> Option<(&Self, Pat, Option<&Expr>, bool)> {
		let group = match &self.inner {
			RuleInnerMatch::Named(_, g) => g,
			RuleInnerMatch::Unnamed(g) => g,
			RuleInnerMatch::Unit(g) => g,
		};
		let first = match group {
			SplitRule::AllPNM(groups) => {
				if let &[ref single] = &**groups { single } else { return None }
			}
			SplitRule::Single { pre_PNM, group, ..} => {
				if pre_PNM.is_empty() { &**group } else { return None }
			},
			SplitRule::Other { .. } => return None,
		};
		if let Group::Literal(ref v, _) = first {
			match v {
				Value::Single(e) => {
					Some((self, Pat::Verbatim(e.to_token_stream()), None, false))
				}
				Value::Save { group: SaveType::Other { pattern, default }, .. } => {
					Some((self, pattern.clone(), default.as_ref(), true))
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
					SplitRule::AllPNM(_) => {
						todo!()
					}
					SplitRule::Single { .. } => {
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
					SplitRule::Single { .. } | SplitRule::AllPNM(_) => {
						if args.is_empty() {
							match (wrapped, saves) {
								(true, true) => quote!{ __next_unwrapped @ #p => Ok(cflp::NodeWrapper { start, end, node: #var(__next_unwrapped.clone()) }) },
								(true, false) => quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var }) },
								(false, true) => quote!{ __next_unwrapped @ #p => Ok(#var(__next_unwrapped.clone())) },
								(false, false) => quote!{ #p => Ok(#var) }
							}
						} else {
							match (wrapped, args.len() > 1) {
								(true, true) => quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var((#(#args.clone()),*)) }) },
								(true, false) => quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var(#(#args.clone()),*) }) },
								(false, true) => quote!{ #p => Ok(#var((#(#args.clone()),*))) },
								(false, false) => quote!{ #p => Ok(#var(#(#args.clone()),*)) },
							}
						}
					}
					SplitRule::Other { .. } => {
						todo!()
					}
				}
			}
			RuleInnerMatch::Unit(g) => {
				match g {
					SplitRule::AllPNM(_) | SplitRule::Single { .. } => {
						if wrapped {
							quote!{ #p => Ok(cflp::NodeWrapper { start, end, node: #var }) }
						} else {
							quote!{ #p => Ok(#var) }
						}
					}
					SplitRule::Other { .. } => unreachable!()
				}
			}
		}
	}
}
