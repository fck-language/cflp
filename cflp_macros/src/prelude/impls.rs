use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::Lifetime;
use crate::prelude::{Group, Meta, PositionType, ReturnType, RuleInner, RuleInnerMatch, SplitRule, Value};

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
	pub fn build(&self, return_type: ReturnType, meta: &Meta) -> (TokenStream, TokenStream) {
		// we need to include this because it will be continually changed, even if it may never be read
		let mut out = quote!{ let mut consumed_tokens = false; };
		let final_return;
		match &self.inner {
			RuleInnerMatch::Named(names_vec, inner) => {
				final_return = quote!{ { #(#names_vec),* } };
				let mut names = names_vec.iter();
				match inner {
					SplitRule::AllPNM(groups) => {
						if meta.is_wrapped {
							for group in groups {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::StartEnd, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta))
								}
								out.extend(quote!{ ; });
							}
						} else {
							for group in groups {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save(n, &self.name, return_type, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta))
								}
								out.extend(quote!{ ; });
							}
						}
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						if meta.is_wrapped {
							for group in pre_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::Start, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::Start, meta))
								}
								out.extend(quote!{ ; });
							}
							if group.contains_save() {
								let n = names.next().unwrap();
								out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::StartEnd, meta))
							} else {
								out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta))
							}
							out.extend(quote!{ ; });
							for group in post_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::End, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::End, meta))
								}
								out.extend(quote!{ ; });
							}
						} else {
							for group in pre_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save(n, &self.name, return_type, meta))
								} else {
									out.extend(group.build_no_save(return_type, meta))
								}
								out.extend(quote!{ ; });
							}
							if group.contains_save() {
								let n = names.next().unwrap();
								out.extend(group.build_save(n, &self.name, return_type, meta))
							} else {
								out.extend(group.build_no_save(return_type, meta))
							}
							out.extend(quote!{ ; });
							for group in post_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save(n, &self.name, return_type, meta))
								} else {
									out.extend(group.build_no_save(return_type, meta))
								}
								out.extend(quote!{ ; });
							}
						}
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						if meta.is_wrapped {
							let start_save_position = if pre_PNM.is_empty() { PositionType::Start } else { PositionType::Start };
							for group in pre_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::Start, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								out.extend(quote!{ ; });
							}
							if start.contains_save() {
								let n = names.next().unwrap();
								out.extend(start.build_save_start_end(n, &self.name, return_type, start_save_position, meta));
							} else {
								out.extend(start.build_no_save_start_end(return_type, PositionType::Start, meta));
							}
							out.extend(quote!{ ; });
						} else {
							for group in pre_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save(n, &self.name, return_type, meta))
								} else {
									out.extend(group.build_no_save(return_type, meta));
								}
								out.extend(quote!{ ; });
							}
							if start.contains_save() {
								let n = names.next().unwrap();
								out.extend(start.build_save(n, &self.name, return_type, meta));
							} else {
								out.extend(start.build_no_save(return_type, meta));
							}
							out.extend(quote!{ ; });
						}
						for i in middle.iter() {
							if i.contains_save() {
								let n = names.next().unwrap();
								out.extend(i.build_save(n, &self.name, return_type, meta));
							} else {
								out.extend(i.build_no_save(return_type, meta));
							}
							out.extend(quote!{ ; });
						}

						if meta.is_wrapped {
							if end.contains_save() {
								let n = names.next().unwrap();
								out.extend(end.build_save_start_end(n, &self.name, return_type, PositionType::End, meta));
							} else {
								out.extend(end.build_no_save_start_end(return_type, PositionType::End, meta));
							}
							out.extend(quote!{ ; });
							for group in post_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save_start_end(n, &self.name, return_type, PositionType::End, meta))
								} else {
									out.extend(group.build_no_save_start_end(return_type, PositionType::End, meta));
								}
								out.extend(quote!{ ; });
							}
						} else {
							if end.contains_save() {
								let n = names.next().unwrap();
								out.extend(end.build_save(n, &self.name, return_type, meta));
							} else {
								out.extend(end.build_no_save(return_type, meta));
							}
							out.extend(quote!{ ; });
							for group in post_PNM {
								if group.contains_save() {
									let n = names.next().unwrap();
									out.extend(group.build_save(n, &self.name, return_type, meta))
								} else {
									out.extend(group.build_no_save(return_type, meta));
								}
								out.extend(quote!{ ; });
							}
						}
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
				#[cfg(not(debug_assertions))]
				todo!("Include pre_PNM and post_PNM groups");
				match inner {
					SplitRule::AllPNM(groups) => {
						let mut k = 0usize;
						groups.iter().for_each(|g| {
							let extend = match (g.contains_save(), meta.is_wrapped) {
								(true, true) => {
									let out = g.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta);
									k += 1;
									out
								},
								(true, false) => {
									let out = g.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta);
									k += 1;
									out
								},
								(false, true) => g.build_no_save_start_end(return_type, PositionType::StartEnd, meta),
								(false, false) => g.build_no_save(return_type, meta),
							};
							out.extend(extend);
							out.extend(quote!{ ; });
						})
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						let mut k = 0usize;
						for g in pre_PNM {
							if g.contains_save() {
								if meta.is_wrapped {
									out.extend(g.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
								}
								k += 1;
							} else {
								if meta.is_wrapped {
									out.extend(g.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_no_save(return_type, meta));
								}
							}
							out.extend(quote!{ ; });
						}
						if group.contains_save() {
							if meta.is_wrapped {
								out.extend(group.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta));
							} else {
								out.extend(group.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
							}
						} else {
							if meta.is_wrapped {
								out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
							} else {
								out.extend(group.build_no_save(return_type, meta));
							}
						}
						out.extend(quote!{ ; });
						for g in post_PNM {
							if g.contains_save() {
								if meta.is_wrapped {
									out.extend(g.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
								}
								k += 1;
							} else {
								if meta.is_wrapped {
									out.extend(g.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_no_save(return_type, meta));
								}
							}
							out.extend(quote!{ ; });
						}
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						let mut k = 0usize;
						for g in pre_PNM {
							if g.contains_save() {
								if meta.is_wrapped {
									out.extend(g.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
								}
								k += 1;
							} else {
								if meta.is_wrapped {
									out.extend(g.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_no_save(return_type, meta));
								}
							}
							out.extend(quote!{ ; });
						}
						if start.contains_save() {
							if meta.is_wrapped {
								out.extend(start.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::Start, meta));
							} else {
								out.extend(start.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
							}
							k += 1;
						} else {
							if meta.is_wrapped {
								out.extend(start.build_no_save_start_end(return_type, PositionType::Start, meta));
							} else {
								out.extend(start.build_no_save(return_type, meta));
							}
						}
						out.extend(quote!{ ; });
						for i in middle.iter() {
							if i.contains_save() {
								out.extend(i.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
								k += 1;
							} else {
								out.extend(i.build_no_save(return_type, meta));
							}
							out.extend(quote!{ ; });
						}
						if end.contains_save() {
							if meta.is_wrapped {
								out.extend(end.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::End, meta));
							} else {
								out.extend(end.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
							}
						} else {
							if meta.is_wrapped {
								out.extend(end.build_no_save_start_end(return_type, PositionType::End, meta));
							} else {
								out.extend(end.build_no_save(return_type, meta));
							}
						}
						out.extend(quote!{ ; });
						for g in post_PNM {
							if g.contains_save() {
								if meta.is_wrapped {
									out.extend(g.build_save_start_end(&format_ident!("v_{}", k), &self.name, return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_save(&format_ident!("v_{}", k), &self.name, return_type, meta));
								}
								k += 1;
							} else {
								if meta.is_wrapped {
									out.extend(g.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								} else {
									out.extend(g.build_no_save(return_type, meta));
								}
							}
							out.extend(quote!{ ; });
						}
					}
				}
			}
			RuleInnerMatch::Unit(inner) => {
				final_return = quote!{};
				match inner {
					SplitRule::AllPNM(groups) => {
						if meta.is_wrapped {
							groups.iter().for_each(|group| {
								out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								out.extend(quote!{ ; });
							});
						} else {
							groups.iter().for_each(|group| {
								out.extend(group.build_no_save(return_type, meta));
								out.extend(quote!{ ; });
							});
						}
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						// don't need to check if it's a save because we can't have any
						if meta.is_wrapped {
							for g in pre_PNM.iter() {
								out.extend(g.build_no_save_start_end(return_type, PositionType::Start, meta));
								out.extend(quote!{ ; })
							}
							out.extend(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
							out.extend(quote!{ ; });
							for g in post_PNM.iter() {
								out.extend(g.build_no_save_start_end(return_type, PositionType::End, meta));
								out.extend(quote!{ ; })
							}
						} else {
							for g in pre_PNM.iter() {
								out.extend(g.build_no_save(return_type, meta));
								out.extend(quote!{ ; })
							}
							out.extend(group.build_no_save(return_type, meta));
							out.extend(quote!{ ; });
							for g in post_PNM.iter() {
								out.extend(g.build_no_save(return_type, meta));
								out.extend(quote!{ ; })
							}
						}
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						if meta.is_wrapped {
							for g in pre_PNM.iter() {
								out.extend(g.build_no_save_start_end(return_type, PositionType::Start, meta));
								out.extend(quote!{ ; })
							}
							out.extend(start.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
							out.extend(quote!{ ; });
							for g in middle.iter() {
								out.extend(g.build_no_save_start_end(return_type, PositionType::Start, meta));
								out.extend(quote!{ ; })
							}
							out.extend(end.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
							out.extend(quote!{ ; });
							for g in post_PNM.iter() {
								out.extend(g.build_no_save_start_end(return_type, PositionType::End, meta));
								out.extend(quote!{ ; })
							}
						} else {
							for g in pre_PNM.iter() {
								out.extend(g.build_no_save(return_type, meta));
								out.extend(quote!{ ; })
							}
							out.extend(start.build_no_save(return_type, meta));
							out.extend(quote!{ ; });
							for g in middle.iter() {
								out.extend(g.build_no_save(return_type, meta));
								out.extend(quote! { ; })
							}
							out.extend(end.build_no_save(return_type, meta));
							out.extend(quote!{ ; });
							for g in post_PNM.iter() {
								out.extend(g.build_no_save(return_type, meta));
								out.extend(quote!{ ; })
							}
						}

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
			RuleInnerMatch::Unnamed(r) => r,
			RuleInnerMatch::Unit(r) => r,
		};
		rules.count_matches()
	}
}

impl SplitRule {
	/// Count the number of match groups
	pub fn count_matches(&self) -> usize {
		match self {
			SplitRule::AllPNM(groups) => groups.iter().map(|g| g.count_matches()).sum::<usize>(),
			SplitRule::Single { pre_PNM, group, post_PNM } => {
				pre_PNM.iter().map(|g| g.count_matches()).sum::<usize>() +
					group.count_matches() +
					post_PNM.iter().map(|g| g.count_matches()).sum::<usize>()
			}
			SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
				pre_PNM.iter().map(|g| g.count_matches()).sum::<usize>() +
					start.count_matches() + middle.iter().map(|g| g.count_matches()).sum::<usize>() + end.count_matches() +
					post_PNM.iter().map(|g| g.count_matches()).sum::<usize>()
			}
		}
	}
}

impl From<Vec<Group>> for SplitRule {
	fn from(value: Vec<Group>) -> Self {
		macro_rules! is_pnm {
		    ($g:expr) => {match $g {
			    Group::Kleene(_, _) | Group::Option(_, _) => true,
				_ => false
		    }};
		}
		if let [group] = &*value {
			if is_pnm!(&group) {
				SplitRule::AllPNM(vec![group.clone()])
			} else {
				SplitRule::Single {
					pre_PNM: Vec::new(),
					group: Box::new(group.clone()),
					post_PNM: Vec::new(),
				}
			}
		} else {
			if let Some(first_index) = value.iter().position(|t| !is_pnm!(t)) {
				let last_index = value.iter().rposition(|t| !is_pnm!(t)).unwrap();
				let (pre_mid, post) = value.split_at(last_index + 1);
				let (pre, mid) = pre_mid.split_at(first_index);
				if let [mid_single] = mid {
					SplitRule::Single {
						pre_PNM: pre.to_vec(),
						group: Box::new(mid_single.clone()),
						post_PNM: post.to_vec()
					}
				} else {
					let (last, rem) = mid.split_last().unwrap();
					let (first, mid) = rem.split_first().unwrap();
					SplitRule::Other {
						pre_PNM: pre.to_vec(),
						start: Box::new(first.clone()),
						middle: mid.to_vec(),
						end: Box::new(last.clone()),
						post_PNM: post.to_vec()
					}
				}
			} else {
				SplitRule::AllPNM(value)
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
	#[inline(always)]
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
