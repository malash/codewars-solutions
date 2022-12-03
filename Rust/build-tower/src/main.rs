fn main() {
    for line in tower_builder(6) {
        println!("{}", line);
    }
}

/// https://www.codewars.com/kata/576757b1df89ecf5bd00073b/train/rust
fn tower_builder(n: usize) -> Vec<String> {
    (0..n)
        .map(|i| {
            format!(
                "{}{}{}",
                " ".repeat(n - i - 1),
                "*".repeat(2 * i + 1),
                " ".repeat(n - i - 1)
            )
        })
        .collect()
}

// Add your tests here.
// See https://doc.rust-lang.org/stable/rust-by-example/testing/unit_testing.html

#[cfg(test)]
mod tests {
    use super::tower_builder;

    #[test]
    fn fixed_tests() {
        assert_eq!(tower_builder(1), vec!["*"]);
        assert_eq!(tower_builder(2), vec![" * ", "***"]);
        assert_eq!(tower_builder(3), vec!["  *  ", " *** ", "*****"]);
    }
}
