fn main() {
    println!("{}", rot13("n"));
}

/// https://www.codewars.com/kata/530e15517bc88ac656000716/train/rust
fn rot13(message: &str) -> String {
    message
        .chars()
        .map(|c| match c {
            'a'..='z' => ((((c as u8) - b'a' + 13) % 26) + b'a') as char,
            'A'..='Z' => ((((c as u8) - b'A' + 13) % 26) + b'A') as char,
            _ => c,
        })
        .collect()
}

// Add your tests here.
// See https://doc.rust-lang.org/stable/rust-by-example/testing/unit_testing.html

#[cfg(test)]
mod tests {
    use super::rot13;

    const ERR_MSG: &str = "\nYour result (left) did not match the expected output (right)";

    fn dotest(s: &str, expected: &str) {
        assert_eq!(rot13(s), expected, "{ERR_MSG} with message = \"{s}\"")
    }

    #[test]
    fn sample_tests() {
        dotest("test", "grfg");
        dotest("Test", "Grfg");
    }
}
