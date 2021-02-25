use std::io;

fn main() {
    let mut str = String::new();
    io::stdin().read_line(&mut str).unwrap();
    let mut ws = str.split_whitespace();
    let n: i32 = ws.next().unwrap().parse().unwrap();
    let a: i32 = ws.next().unwrap().parse().unwrap();
    let b: i32 = ws.next().unwrap().parse().unwrap();
    let mut r = 0;
    for x in 1..(n + 1) {
        let s = dsum(x);
        if s >= a && s <= b {
            r += x;
        }
    }
    println!("{:?}", r);
}

fn dsum(mut x: i32) -> i32 {
    let mut r = 0;
    while (x > 0) {
        r += x % 10;
        x /= 10
    }
    r
}
