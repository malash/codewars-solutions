fn main() {
    println!("{}", solution("camelCasing"));
}

/// https://www.codewars.com/kata/5208f99aee097e6552000148/train/rust
fn solution(s: &str) -> String {
    s.chars()
        .map(|c| format!("{}{}", if c.is_ascii_uppercase() { " " } else { "" }, c))
        .collect()
}

// Add your tests here.
// See https://doc.rust-lang.org/stable/rust-by-example/testing/unit_testing.html

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solution() {
        assert_eq!(solution("camelCasing"), "camel Casing");
        assert_eq!(solution("camelCasingTest"), "camel Casing Test");
    }
}
