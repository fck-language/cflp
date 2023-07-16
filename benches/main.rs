use criterion::{criterion_group, criterion_main};

mod complex;
mod simple;

criterion_group!(simple_parse, simple::main);
criterion_group!(complex_parse, complex::main);
criterion_main!(simple_parse, complex_parse);
