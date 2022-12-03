fn main() {
    println!("{:?}", divisors(15));
}

/// https://www.codewars.com/kata/544aed4c4a30184e960010f4/train/rust
fn divisors(integer: u32) -> Result<Vec<u32>, String> {
    let mut numbers: Vec<u32> = Vec::new();
    for i in 2..integer {
        if integer % i == 0 {
            numbers.push(i);
        }
    }
    match numbers.len() {
        0 => Err(format!("{} is prime", integer).to_string()),
        _ => Ok(numbers),
    }
}

#[test]
fn tests() {
    assert_eq!(divisors(15), Ok(vec![3, 5]));
    assert_eq!(divisors(12), Ok(vec![2, 3, 4, 6]));
    assert_eq!(divisors(13), Err("13 is prime".to_string()));
}
