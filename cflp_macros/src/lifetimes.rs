use std::collections::HashSet;

use syn::{GenericArgument, Lifetime, LifetimeDef, PathArguments, Type, TypeParamBound};

use crate::{
	prelude::{Group, Rule, RuleInner, RuleInnerEnum, Value},
	saving::{MatchArg, SaveType},
};

pub trait Lifetimes {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>);
}

impl<T: Lifetimes> Lifetimes for Vec<T> {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		for i in self.iter() {
			i.lifetimes(comp_type, lifetimes)
		}
	}
}

impl<'a, T: Lifetimes> Lifetimes for syn::punctuated::Iter<'a, T> {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		for i in self.clone().into_iter() {
			i.lifetimes(comp_type, lifetimes)
		}
	}
}

impl Lifetimes for Rule {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		self.inner.lifetimes(comp_type, lifetimes)
	}
}

impl Lifetimes for RuleInnerEnum {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			RuleInnerEnum::Single(s) => s.lifetimes(comp_type, lifetimes),
			RuleInnerEnum::Multiple(m) => m.lifetimes(comp_type, lifetimes),
		}
	}
}

impl Lifetimes for RuleInner {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		self.inner.lifetimes(comp_type, lifetimes)
	}
}

impl Lifetimes for Value {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Value::Save(s) => s.lifetimes(comp_type, lifetimes),
			Value::Group(g, _) => g.lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for Group {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Group::Literal(v, _) | Group::Kleene(v, _) | Group::Positive(v, _) | Group::Option(v, _) => {
				v.lifetimes(comp_type, lifetimes)
			}
		}
	}
}

impl Lifetimes for SaveType {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			SaveType::Call(_, args) => lifetimes.extend(args.clone()),
			SaveType::Match(t, constraints) => {
				if constraints.iter().position(|t| t.is_type()).is_some() {
					t.lifetimes(comp_type, lifetimes);
					constraints.lifetimes(comp_type, lifetimes)
				} else {
					comp_type.lifetimes(comp_type, lifetimes)
				}
			}
		}
	}
}

impl Lifetimes for MatchArg {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			MatchArg::Type(t) => t.lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for Type {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Type::Array(a) => a.elem.lifetimes(comp_type, lifetimes),
			Type::Group(g) => g.elem.lifetimes(comp_type, lifetimes),
			Type::Paren(p) => p.elem.lifetimes(comp_type, lifetimes),
			Type::Path(p) => {
				if let Some(qpath) = &p.qself {
					qpath.ty.lifetimes(comp_type, lifetimes)
				}
				for seg in p.path.segments.iter() {
					if let PathArguments::AngleBracketed(args) = &seg.arguments {
						args.args.iter().lifetimes(comp_type, lifetimes)
					}
				}
			}
			Type::Reference(r) => {
				if let Some(l) = &r.lifetime {
					lifetimes.insert(l.clone());
				}
			}
			Type::Slice(s) => s.elem.lifetimes(comp_type, lifetimes),
			Type::Tuple(t) => t.elems.iter().lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for GenericArgument {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			GenericArgument::Lifetime(l) => {
				lifetimes.insert(l.clone());
			}
			GenericArgument::Type(t) => t.lifetimes(comp_type, lifetimes),
			GenericArgument::Binding(b) => b.ty.lifetimes(comp_type, lifetimes),
			GenericArgument::Constraint(c) => {
				for bound in c.bounds.iter() {
					match bound {
						TypeParamBound::Trait(t) => {
							if let Some(bl) = &t.lifetimes {
								bl.lifetimes.iter().lifetimes(comp_type, lifetimes)
							}
						}
						TypeParamBound::Lifetime(l) => {
							lifetimes.insert(l.clone());
						}
					}
				}
			}
			_ => {}
		}
	}
}

impl Lifetimes for LifetimeDef {
	fn lifetimes(&self, _comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		lifetimes.insert(self.lifetime.clone());
	}
}
