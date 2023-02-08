use criterion::{criterion_group, criterion_main, Criterion};
use nolana::parser::Parser;
use std::hint::black_box;

pub fn parsing(c: &mut Criterion) {
    c.bench_function("Parsing", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(
                "math.pow(1, math.pow(2, math.pow(3, 4 * math.sin(5 * 6))))",
            ));
            parser.parse_program().expect("should parse");
        })
    });
}

criterion_group!(benches, parsing);
criterion_main!(benches);
