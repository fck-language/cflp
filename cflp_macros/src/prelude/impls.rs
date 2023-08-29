use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Lifetime, Type};
use crate::prelude::{Group, PositionType, ReturnType, RuleInner, RuleInnerMatch, SplitRule, Value};

impl ReturnType {
	/// Make a new non-conflicting lifetime
	pub fn new_lifetime(&self, wrap_result: bool) -> Self {
		match self {
			ReturnType::Function => Self::Lifetime(0, wrap_result),
			ReturnType::Lifetime(i, _) => ReturnType::Lifetime(i + 1, wrap_result)
		}
	}
	
	/// Set if the lifetime should wrap it's result
	pub fn set_wrapped(self, t: bool) -> Self {
		match self {
			ReturnType::Function => self,
			ReturnType::Lifetime(i, _) => Self::Lifetime(i, t)
		}
	}
	
	/// Check if the return type should be wrapped
	pub fn is_wrapped(&self) -> bool {
		match self {
			ReturnType::Function => false,
			ReturnType::Lifetime(_, t) => *t
		}
	}
	
	/// Returns a `TokenStream` of the current lifetime
	pub fn get_lifetime(&self) -> TokenStream {
		match self {
			ReturnType::Function => TokenStream::new(),
			ReturnType::Lifetime(i, _) => {
				let lifetime = Lifetime::new(&*format!("'l{}", i), Span::mixed_site());
				quote!{ #lifetime }
			}
		}
	}
	
	/// Builds a `TokenStream` to return the given `TokenStream`
	pub fn to_token_stream(&self, inner: TokenStream) -> TokenStream {
		match self {
			ReturnType::Function => quote!{ return #inner },
			ReturnType::Lifetime(i, _) => {
				let l = Lifetime::new(&*format!("'l{}", i), Span::mixed_site());
				if inner.is_empty() {
					quote!{ break #l }
				} else { quote!{ break #l #inner } }
			}
		}
	}
}

impl RuleInner {
	/// Impl generation entry point
	pub fn build(&self, return_type: ReturnType, comp_type: &Type, map: bool, wrapped: bool) -> (TokenStream, TokenStream) {
		let mut out = TokenStream::new();
		let final_return;
		match &self.inner {
			RuleInnerMatch::Named(names_vec, inner) => {
				final_return = quote!{ { #(#names_vec),* } };
				let mut names = names_vec.iter();
				match inner {
					SplitRule::Single(g) => {
						if g.contains_save() {
							let n = names.next().unwrap();
							if wrapped {
								out.extend(g.build_save_start_end(
									n.clone(), &self.name, return_type, comp_type, map,
									wrapped, PositionType::StartEnd
								));
							} else {
								out.extend(g.build_save(
									n.clone(), &self.name, return_type, comp_type, map, wrapped, true
								));
							}
						} else {
							if wrapped {
								out.extend(g.build_no_save_start_end(
									return_type, comp_type, map, PositionType::StartEnd
								));
							} else {
								out.extend(g.build_no_save(
									return_type, comp_type, map, true
								));
							}
						}
						out.extend(quote!{ ; });
					}
					SplitRule::Other { start, middle, end } => {
						if start.contains_save() {
							let n = names.next().unwrap();
							if wrapped {
								out.extend(start.build_save_start_end(
									n.clone(), &self.name, return_type, comp_type, map,
									wrapped, PositionType::Start
								));
							} else {
								out.extend(start.build_save(
									n.clone(), &self.name, return_type, comp_type, map, wrapped, true
								));
							}
						} else {
							if wrapped {
								out.extend(start.build_no_save_start_end(
									return_type, comp_type, map, PositionType::Start
								));
							} else {
								out.extend(start.build_no_save(
									return_type, comp_type, map, true
								));
							}
						}
						out.extend(quote!{ ; });
						for i in middle.iter() {
							if i.contains_save() {
								let n = names.next().unwrap();
								out.extend(i.build_save(
									n.clone(), &self.name, return_type, comp_type, map, wrapped, false
								));
							} else {
								out.extend(i.build_no_save(
									return_type, comp_type, map, false
								));
							}
							out.extend(quote!{ ; });
						}
						if end.contains_save() {
							let n = names.next().unwrap();
							if wrapped {
								out.extend(end.build_save_start_end(
									n.clone(), &self.name, return_type, comp_type, map,
									wrapped, PositionType::End
								));
							} else {
								out.extend(end.build_save(
									n.clone(), &self.name, return_type, comp_type, map, wrapped, false
								));
							}
						} else {
							if wrapped {
								out.extend(end.build_no_save_start_end(
									return_type, comp_type, map, PositionType::End
								));
							} else {
								out.extend(end.build_no_save(
									return_type, comp_type, map, false
								));
							}
						}
						out.extend(quote!{ ; });
					}
				}
			}
			RuleInnerMatch::Unnamed(inner) => {
				let count = inner.count_matches();
				final_return = if count == 0 {
					quote!{}
				} else {
					let args = (0..count).map(|t| format_ident!("v_{}", t));
					quote!{ (#(#args),*) }
				};
				match inner {
					SplitRule::Single(g) => {
						if g.contains_save() {
							if wrapped {
								out.extend(g.build_save_start_end(
									format_ident!("v_0"), &self.name, return_type, comp_type, map,
									wrapped, PositionType::StartEnd
								));
							} else {
								out.extend(g.build_save(
									format_ident!("v_0"), &self.name, return_type, comp_type, map,
									wrapped, true
								));
							}
						} else {
							if wrapped {
								out.extend(g.build_no_save_start_end(
									return_type, comp_type, map, PositionType::StartEnd
								));
							} else {
								out.extend(g.build_no_save(
									return_type, comp_type, map, true
								));
							}
						}
						out.extend(quote!{ ; });
					}
					SplitRule::Other { start, middle, end } => {
						let mut k = 0usize;
						if start.contains_save() {
							if wrapped {
								out.extend(start.build_save_start_end(
									format_ident!("v_0"), &self.name, return_type, comp_type, map,
									wrapped, PositionType::Start
								));
							} else {
								out.extend(start.build_save(
									format_ident!("v_0"), &self.name, return_type, comp_type, map,
									wrapped, true
								));
							}
							k = 1;
						} else {
							if wrapped {
								out.extend(start.build_no_save_start_end(
									return_type, comp_type, map, PositionType::Start
								));
							} else {
								out.extend(start.build_no_save(
									return_type, comp_type, map, true
								));
							}
						}
						out.extend(quote!{ ; });
						for i in middle.iter() {
							if i.contains_save() {
								out.extend(i.build_save(
									format_ident!("v_{}", k), &self.name, return_type, comp_type, map, wrapped, false
								));
								k += 1;
							} else {
								out.extend(i.build_no_save(
									return_type, comp_type, map, false
								));
							}
							out.extend(quote!{ ; });
						}
						if end.contains_save() {
							if wrapped {
								out.extend(end.build_save_start_end(
									format_ident!("v_{}", k), &self.name, return_type, comp_type, map,
									wrapped, PositionType::End
								));
							} else {
								out.extend(end.build_save(
									format_ident!("v_{}", k), &self.name, return_type, comp_type, map,
									wrapped, false
								));
							}
						} else {
							if wrapped {
								out.extend(end.build_no_save_start_end(
									return_type, comp_type, map, PositionType::End
								));
							} else {
								out.extend(end.build_no_save(
									return_type, comp_type, map, false
								));
							}
						}
						out.extend(quote!{ ; });
					}
				}
			}
		}
		(out, final_return)
	}
}

impl RuleInnerMatch {
	/// Count the number of match groups
	pub fn count_matches(&self) -> usize {
		let rules = match self {
			RuleInnerMatch::Named(_, r) => r,
			RuleInnerMatch::Unnamed(r) => r
		};
		rules.count_matches()
	}
}

impl SplitRule {
	/// Count the number of match groups
	pub fn count_matches(&self) -> usize {
		match self {
			SplitRule::Single(inner) => inner.count_matches(),
			SplitRule::Other { start, middle, end } => {
				start.count_matches() + middle.iter().map(|g| g.count_matches()).sum::<usize>() + end.count_matches()
			}
		}
	}
}

impl Value {
	/// Returns if the value is or has a nested `Value::Save`
	pub fn contains_save(&self) -> bool {
		match self {
			Value::Single(_) => false,
			Value::Call(_) => false,
			Value::Save { .. } => true,
			Value::Group(_, s) => *s
		}
	}
	
	/// Count the number of match groups
	pub fn count_matches(&self) -> usize {
		match self {
			Value::Save { .. } => 1,
			Value::Group(group, true) => group.count_matches(),
			_ => 0
		}
	}
}

impl Group {
	/// Returns if the value is or has a nested `Value::Save`
	pub fn contains_save(&self) -> bool {
		match self {
			Group::Literal(_, t) | Group::Kleene(_, t) | Group::Positive(_, t) | Group::Option(_, t) => *t
		}
	}
	
	/// Count the number of match groups
	pub fn count_matches(&self) -> usize {
		match self {
			Group::Literal(v, true) => v.count_matches(),
			Group::Kleene(v, true) => if v.count_matches() == 0 { 0 } else { 1 },
			Group::Positive(v, true) => if v.count_matches() == 0 { 0 } else { 1 },
			Group::Option(v, true) => if v.count_matches() == 0 { 0 } else { 1 },
			_ => 0
		}
	}
}
