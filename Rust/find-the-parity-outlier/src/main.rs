fn main() {
    let values = [2, 6, 8, -10, 3];
    println!("{}", find_outlier(&values[..]));
}

/// https://www.codewars.com/kata/5526fc09a1bbd946250002dc/train/rust
fn find_outlier(values: &[i32]) -> i32 {
    let mut count_odd = 0;
    let mut count_even = 0;
    let mut last_odd = 0;
    let mut last_even = 0;
    for value in values {
        match value % 2 {
            1 | -1 => {
                count_odd += 1;
                last_odd = *value;
            }
            0 => {
                count_even += 1;
                last_even = *value;
            }
            _ => continue,
        }
    }
    if count_odd > count_even {
        last_even
    } else {
        last_odd
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let t1 = [2, 6, 8, -10, 3];
        let t2 = [
            206847684, 1056521, 7, 17, 1901, 21104421, 7, 1, 35521, 1, 7781,
        ];
        let t3 = [std::i32::MAX, 0, 1];
        assert_eq!(3, find_outlier(&t1));
        assert_eq!(206847684, find_outlier(&t2));
        assert_eq!(0, find_outlier(&t3));
    }
}
