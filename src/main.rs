use std::fs;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    /// The original file
    original: String,
    /// The edited file
    new: String,
}

mod utils {
    use std::fmt;

    type Lines<'a> = Vec<(usize, &'a str)>;

    #[derive(PartialEq, Debug, Clone)]
    pub enum ChangeType {
        Removed,
        Added,
    }

    impl fmt::Display for ChangeType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Self::Added => "+",
                    Self::Removed => "-",
                }
            )
        }
    }

    /// A wrapper for `Vec<Line>`
    pub struct LinesDiff {
        pub lines: Vec<Line>,
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Line {
        pub number: usize,
        pub length: usize,
        pub change: ChangeType,
    }

    impl fmt::Display for LinesDiff {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                self.lines
                    .iter()
                    .map(|item| format!("{}{}\n", item.change, item.number))
                    .collect::<String>()
                    .trim_end()
            )
        }
    }

    pub fn diff<S>(original_string: S, new_string: S) -> LinesDiff
    where
        S: Into<String>,
    {
        let original_string: String = original_string.into();
        let new_string: String = new_string.into();

        let original: Lines = original_string.lines().enumerate().collect();
        let mut new: Lines = new_string.lines().enumerate().collect();

        let mut changes: Vec<Line> = Vec::new();

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
                changes.push(Line {
                    number: original_index,
                    length: 1,
                    change: ChangeType::Removed,
                })
            }
        }

        for (new_index, _new_line) in new {
            changes.push(Line {
                number: new_index,
                length: 1,
                change: ChangeType::Added,
            })
        }

        for (index, line_change) in changes.clone().iter().enumerate() {
            if index < changes.len() - 1 {
                for other_index in index + 1..changes.len() {
                    if line_change.number + other_index - index == changes[other_index].number {
                        changes[index].length += 1;
                        changes.remove(other_index);
                    } else {
                        continue;
                    }
                }
            }
        }

        changes.sort_by(|a, b| a.number.cmp(&b.number));

        LinesDiff { lines: changes }
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{self, ChangeType, Line};

    type TestPair = ((&'static str, &'static str), &'static [Line]);

    const TEST_PAIRS: &'static [TestPair] = &[(
        (
            &"first line\nsecond line\nthird line",
            &"new first line\nsecond line\nsomething changed here\nlast line",
        ),
        &[
            Line {
                number: 0,
                length: 1,
                change: ChangeType::Removed,
            },
            Line {
                number: 0,
                length: 1,
                change: ChangeType::Added,
            },
            Line {
                number: 2,
                length: 1,
                change: ChangeType::Removed,
            },
            Line {
                number: 2,
                length: 2,
                change: ChangeType::Added,
            },
        ],
    )];

    #[test]
    fn test_with_pairs() {
        for pair in TEST_PAIRS {
            let diff = utils::diff(pair.0 .0, pair.0 .1);
            assert_eq!(diff.lines.iter().as_slice(), pair.1)
        }
    }
}

fn main() {
    let args = Args::parse();

    let original_content = fs::read_to_string(args.original)
        .expect("Couldn't open original file. Check if it exists or if the path supplied is valid");
    let new_content = fs::read_to_string(args.new)
        .expect("Couldn't open new file. Check if it exists or if the path supplied is valid");

    let diff = utils::diff(original_content, new_content);

    println!("{}", diff)
}
