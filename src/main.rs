mod utils {
    type Lines<'a> = Vec<(usize, &'a str)>;

    pub struct LinesDiff {
        pub removed: Vec<usize>,
        pub added: Vec<usize>,
    }

    pub fn diff<S>(original_string: S, new_string: S) -> LinesDiff
    where
        S: Into<String>,
    {
        let original_string: String = original_string.into();
        let new_string: String = new_string.into();

        let original: Lines = original_string.lines().enumerate().collect();
        let mut new: Lines = new_string.lines().enumerate().collect();

        let mut removed: Vec<usize> = Vec::new();

        for (original_index, original_line) in original {
            let mut line_found = false;
            for (new_index, new_line) in new.clone() {
                if original_line.len() != new_line.len() {
                    continue;
                } else if original_line == new_line {
                    line_found = true;
                    new.remove(new_index);
                    break;
                }
            }

            if !line_found {
                removed.push(original_index)
            }
        }

        let mut added: Vec<usize> = Vec::new();

        for (new_index, _new_line) in new {
            added.push(new_index)
        }

        LinesDiff { removed, added }
    }
}

fn main() {}
