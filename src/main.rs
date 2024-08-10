use tokenizer::Tokenizer;

pub mod tokenizer;



fn main() {
    let src = "Hello world += if for else or 13.45 0x123 ?";
    let tok = Tokenizer::new(src.as_bytes()).tokenize();

    for t in tok {
        let range = t.source.clone();
        println!("{:?} - {}", t, &src[range]);
    }

    println!("Hello, world!");
}
