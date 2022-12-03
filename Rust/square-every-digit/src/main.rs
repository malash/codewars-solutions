fn main() {
    println!("{}", square_digits(9119));
}

/// https://www.codewars.com/kata/546e2562b03326a88e000020/train/rust
fn square_digits(num: u64) -> u64 {
    num.to_string()
        .chars()
        .map(|c| {
            let n = c.to_string().parse::<u64>().unwrap();
            n * n
        })
        .map(|n| n.to_string())
        .collect::<Vec<String>>()
        .join("")
        .parse::<u64>()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::square_digits;

    #[test]
    fn test_square_digits() {
        assert_eq!(square_digits(9119), 811181, "\nFailed with num 9119");
    }
}
