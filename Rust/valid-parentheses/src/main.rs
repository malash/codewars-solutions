fn main() {
    println!("{}", valid_parentheses("(}{)"));
}

/// https://www.codewars.com/kata/52774a314c2333f0a7000688/train/rust
fn valid_parentheses(s: &str) -> bool {
    let mut count = 0;
    for c in s.chars() {
        match c {
            '(' => count += 1,
            ')' => count -= 1,
            _ => (),
        };
        if count < 0 {
            return false;
        }
    }
    count == 0
}

#[cfg(test)]
mod tests {
    use super::valid_parentheses;

    #[test]
    fn sample_tests() {
        do_test("()", true);
        do_test("())", false);
        do_test("", true);
        do_test("(}{)", true);
    }

    fn do_test(s: &str, exp: bool) {
        let actual = valid_parentheses(s);
        assert_eq!(
            actual, exp,
            "\nFor the input \"{}\", your result ({}) did not match the expected output ({})",
            s, actual, exp
        );
    }
}
