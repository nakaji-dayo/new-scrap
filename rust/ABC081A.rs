use std::io;

fn main() {
    let mut str = String::new();
    io::stdin().read_line(&mut str);
    str.retain(|x| x == '1');
    print!("{}", str.len());
}
