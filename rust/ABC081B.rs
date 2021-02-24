use std::io;

fn main() {
    let mut n = String::new();
    let mut str = String::new();
    io::stdin().read_line(&mut n);
    io::stdin().read_line(&mut str);
    let ws: Vec<&str> = str.trim().split(' ').collect();
    let res: usize = ws.into_iter().map(parser).min().unwrap_or(0);
    println!("{:?}", res);
}

fn parser(s: &str) -> usize {
    let n = s.parse::<i32>().unwrap();
    if n == 0 {
        return 0;
    }
    let bs = format!("{:b}", n);
    bs.chars().rev().take_while(|b| *b == '0').count()
}
