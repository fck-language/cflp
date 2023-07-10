use std::collections::HashSet;
use syn::{GenericArgument, GenericParam, Lifetime, LifetimeParam, PathArguments, PathSegment, Type, TypeParamBound};
use crate::prelude::{Group, Rules, Value, RuleInner, SaveType, SplitRule};

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

impl Lifetimes for Rules {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Rules::Single(inner) => inner.lifetimes(comp_type, lifetimes),
			Rules::Multiple { first, rem} => {
				first.lifetimes(comp_type, lifetimes);
				for i in rem.iter() {
					i.lifetimes(comp_type, lifetimes)
				}
			}
		}
	}
}

impl Lifetimes for RuleInner {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		self.name.segments.iter().for_each(|p| p.lifetimes(comp_type, lifetimes));
	}
}

impl Lifetimes for PathSegment {
	fn lifetimes(&self, _comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match &self.arguments {
			PathArguments::AngleBracketed(inner) => {
				for part in inner.args.iter() {
					if let GenericArgument::Lifetime(l) = part {
						lifetimes.insert(l.clone());
					}
				}
			}
			_ => {}
		}
	}
}

impl Lifetimes for Value {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Value::Save { group, .. } => group.lifetimes(comp_type, lifetimes),
			Value::Group(g, _) => g.lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for SplitRule {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			SplitRule::Single(inner) => inner.lifetimes(comp_type, lifetimes),
			SplitRule::Other { start, middle, end } => {
				start.lifetimes(comp_type, lifetimes);
				middle.iter().for_each(|t| t.lifetimes(comp_type, lifetimes));
				end.lifetimes(comp_type, lifetimes);
			}
		}
	}
}

impl Lifetimes for Group {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			Group::Literal(v, _)
			| Group::Kleene(v, _)
			| Group::Positive(v, _)
			| Group::Option(v, _) => v.lifetimes(comp_type, lifetimes)
		}
	}
}

impl Lifetimes for SaveType {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		if let SaveType::Call(ref path) = self {
			path.segments.iter().for_each(|s| s.lifetimes(comp_type, lifetimes))
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
			Type::Reference(r) => if let Some(l) = &r.lifetime {
				lifetimes.insert(l.clone());
			},
			Type::Slice(s) => s.elem.lifetimes(comp_type, lifetimes),
			Type::Tuple(t) => t.elems.iter().lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for GenericArgument {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			GenericArgument::Lifetime(l) => { lifetimes.insert(l.clone()); },
			GenericArgument::Type(t) => t.lifetimes(comp_type, lifetimes),
			GenericArgument::AssocType(a) => {
				a.ty.lifetimes(comp_type, lifetimes);
				if let Some(g) = &a.generics {
					g.args.iter().lifetimes(comp_type, lifetimes)
				}
			},
			GenericArgument::Constraint(c) => {
				for bound in c.bounds.iter() {
					match bound {
						TypeParamBound::Trait(t) => {
							if let Some(bl) = &t.lifetimes {
								bl.lifetimes.iter().lifetimes(comp_type, lifetimes)
							}
						}
						TypeParamBound::Lifetime(l) => { lifetimes.insert(l.clone()); }
						_ => {}
					}
				}
			}
			_ => {}
		}
	}
}

impl Lifetimes for GenericParam {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			GenericParam::Lifetime(l) => { lifetimes.insert(l.lifetime.clone()); }
			GenericParam::Type(t) => t.bounds.iter().lifetimes(comp_type, lifetimes),
			_ => {}
		}
	}
}

impl Lifetimes for TypeParamBound {
	fn lifetimes(&self, comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		match self {
			TypeParamBound::Trait(t) => if let Some(l) = &t.lifetimes {
				l.lifetimes.iter().lifetimes(comp_type, lifetimes)
			}
			TypeParamBound::Lifetime(l) => { lifetimes.insert(l.clone()); }
			_ => {}
		}
	}
}

impl Lifetimes for LifetimeParam {
	fn lifetimes(&self, _comp_type: &Type, lifetimes: &mut HashSet<Lifetime>) {
		lifetimes.insert(self.lifetime.clone());
	}
}
