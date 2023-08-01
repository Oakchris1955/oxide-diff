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
    use std::cmp::Ordering;
    use std::fmt;

    #[derive(PartialEq, Eq, Debug, Clone)]
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

    #[derive(PartialEq, Debug)]
    pub struct LCSItem {
        pub original_line: usize,
        pub new_line: usize,
        pub length: usize,
    }

    /// A wrapper for `Vec<Line>`
    pub struct LinesDiff {
        pub lines: Vec<Line>,
        pub lcs_list: Vec<LCSItem>,
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct Line {
        pub next_lcs: usize,
        pub length: usize,
        pub change: ChangeType,
    }

    impl Ord for Line {
        fn cmp(&self, other: &Self) -> Ordering {
            if self.next_lcs == other.next_lcs {
                if self.change == other.change {
                    Ordering::Equal
                } else if self.change == ChangeType::Removed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                self.next_lcs.cmp(&other.next_lcs)
            }
        }
    }

    impl PartialOrd for Line {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(if self.next_lcs == other.next_lcs {
                if self.change == other.change {
                    Ordering::Equal
                } else if self.change == ChangeType::Removed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                self.next_lcs.cmp(&other.next_lcs)
            })
        }
    }

    impl fmt::Display for LinesDiff {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut output = String::new();

            for item in self.lines.iter() {
                let lcs_item =
                    self.lcs_list
                        .iter()
                        .take(item.next_lcs)
                        .last()
                        .unwrap_or(&LCSItem {
                            original_line: 0,
                            new_line: 0,
                            length: 0,
                        });

                for index in 0..item.length {
                    output.push_str(&format!(
                        "{}{}\n",
                        item.change,
                        (match item.change {
                            ChangeType::Added => lcs_item.new_line,
                            ChangeType::Removed => lcs_item.original_line,
                        }) + index
                            + 1
                    ))
                }
            }

            write!(f, "{}", output.trim_end())
        }
    }

    pub fn diff<S>(original_string: S, new_string: S) -> LinesDiff
    where
        S: Into<String>,
    {
        let original_string: String = original_string.into();
        let new_string: String = new_string.into();

        let mut begin_index = 0;

        let mut lcs_list: Vec<LCSItem> = Vec::new();
        let mut deletions: Vec<Line> = Vec::new();
        let mut additions: Vec<Line> = Vec::new();

        for (original_index, original_line) in original_string.lines().enumerate() {
            let mut is_removed = true;

            for (new_index, new_line) in new_string.lines().enumerate().skip(begin_index) {
                if original_line == new_line {
                    if let Some(last_item) = lcs_list.last() {
                        if last_item.original_line + last_item.length == original_index
                            && last_item.new_line + last_item.length == new_index
                        {
                            lcs_list.last_mut().unwrap().length += 1;
                        } else {
                            lcs_list.push(LCSItem {
                                original_line: original_index + 1,
                                new_line: new_index + 1,
                                length: 1,
                            })
                        }
                    } else {
                        lcs_list.push(LCSItem {
                            original_line: original_index + 1,
                            new_line: new_index + 1,
                            length: 1,
                        })
                    }
                    additions.push(Line {
                        next_lcs: lcs_list.len() - 1,
                        length: additions
                            .iter()
                            .rev()
                            .take(2)
                            .last()
                            .unwrap_or(&Line {
                                next_lcs: 0,
                                length: 0,
                                change: ChangeType::Added,
                            })
                            .length
                            + 1,
                        change: ChangeType::Added,
                    });

                    begin_index = new_index;
                    is_removed = false;
                    break;
                }
            }

            if is_removed {
                if let Some(last_change) = deletions.last_mut() {
                    if last_change.next_lcs == lcs_list.len() {
                        last_change.length += 1;
                    } else {
                        deletions.push(Line {
                            next_lcs: lcs_list.len(),
                            length: 1,
                            change: ChangeType::Removed,
                        })
                    }
                } else {
                    deletions.push(Line {
                        next_lcs: lcs_list.len(),
                        length: 1,
                        change: ChangeType::Removed,
                    })
                }
            }
        }

        let mut changes = [deletions, additions].concat();
        changes.sort();

        LinesDiff {
            lines: changes,
            lcs_list,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::*;

    type TestPair = (
        (&'static str, &'static str),
        &'static [Line],
        &'static [LCSItem],
    );

    const TEST_PAIRS: &'static [TestPair] = &[(
        (
            &"first line\nsecond line\nthird line",
            &"new first line\nsecond line\nsomething changed here\nlast line",
        ),
        &[
            Line {
                next_lcs: 0,
                length: 1,
                change: ChangeType::Removed,
            },
            Line {
                next_lcs: 0,
                length: 1,
                change: ChangeType::Added,
            },
            Line {
                next_lcs: 1,
                length: 1,
                change: ChangeType::Removed,
            },
        ],
        &[LCSItem {
            original_line: 2,
            new_line: 2,
            length: 1,
        }],
    )];

    #[test]
    fn check_diff() {
        for pair in TEST_PAIRS {
            let diff = diff(pair.0 .0, pair.0 .1);
            assert_eq!(diff.lines.as_slice(), pair.1);
            assert_eq!(diff.lcs_list.as_slice(), pair.2);
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
