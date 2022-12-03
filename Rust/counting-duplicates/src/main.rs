use std::collections::HashMap;

fn main() {
    println!("{}", count_duplicates("aAbBc"));
}

/// https://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1/train/rust
fn count_duplicates(text: &str) -> u32 {
    let mut sum: HashMap<char, u32> = HashMap::new();
    text.chars().fold(&mut sum, |accum, item| {
        *accum.entry(item.to_ascii_lowercase()).or_insert(0) += 1;
        accum
    });
    sum.iter().fold(0, |result, (_, count)| {
        result + (if *count > 1 { 1 } else { 0 })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abcde() {
        assert_eq!(count_duplicates("abcde"), 0);
    }

    #[test]
    fn test_abcdea() {
        assert_eq!(count_duplicates("abcdea"), 1);
    }

    #[test]
    fn test_indivisibility() {
        assert_eq!(count_duplicates("indivisibility"), 1);
    }
}
