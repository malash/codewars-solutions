fn main() {
    println!("Hello, world!");
}

/// https://www.codewars.com/kata/5592e3bd57b64d00f3000047/train/rust
fn find_nb(m: u64) -> i32 {
    let mut count: u64 = 0;
    for i in 1.. {
        count += (i as u64).pow(3);
        if count > m {
            return -1;
        } else if count == m {
            return i;
        }
    }
    -1
}

#[cfg(test)]
mod sample_tests {
    use super::find_nb;

    fn do_test(n: u64, exp: i32) {
        assert_eq!(
            find_nb(n),
            exp,
            "\nYour result (left) did not match expected output (right) for n={n}"
        );
    }

    #[test]
    fn basics_find_nb() {
        let cases = [
            (4, -1),
            (16, -1),
            (4183059834009, 2022),
            (24723578342962, -1),
            (135440716410000, 4824),
            (40539911473216, 3568),
            (26825883955641, 3218),
        ];
        for (n, expected) in cases {
            do_test(n, expected);
        }
    }
}
