fn main() {
    println!("{}", digital_root(1234));
}

/// https://www.codewars.com/kata/541c8630095125aba6000c00/train/rust
fn digital_root(n: i64) -> i64 {
    let result: i64 = n
        .to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i64)
        .sum();
    match result {
        0..=9 => result,
        _ => digital_root(result),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns_expected() {
        assert_eq!(digital_root(16), 7);
    }
}
