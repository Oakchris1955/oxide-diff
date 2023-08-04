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

    /// A wrapper for `Vec<Changes>`
    pub struct LinesDiff {
        pub line_changes: Vec<LineChange>,
        pub lcs_list: Vec<LCSItem>,
        pub old_text: String,
        pub new_text: String,
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct LineChange {
        pub next_lcs: usize,
        pub length: usize,
        pub change_type: ChangeType,
    }

    impl Ord for LineChange {
        fn cmp(&self, other: &Self) -> Ordering {
            if self.next_lcs == other.next_lcs {
                if self.change_type == other.change_type {
                    Ordering::Equal
                } else if self.change_type == ChangeType::Removed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                self.next_lcs.cmp(&other.next_lcs)
            }
        }
    }

    impl PartialOrd for LineChange {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(if self.next_lcs == other.next_lcs {
                if self.change_type == other.change_type {
                    Ordering::Equal
                } else if self.change_type == ChangeType::Removed {
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

            for change in self.line_changes.iter() {
                let default_item = LCSItem {
                    original_line: self.old_text.lines().count() + 1,
                    new_line: self.new_text.lines().count() + 1,
                    length: 0,
                };

                let lcs_item = self.lcs_list.get(change.next_lcs).unwrap_or(&default_item);

                for index in (0..change.length).rev() {
                    output.push_str(&format!(
                        "{}{}\n",
                        change.change_type,
                        (match change.change_type {
                            ChangeType::Added => lcs_item.new_line,
                            ChangeType::Removed => lcs_item.original_line,
                        }) - index
                            - 1
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
        let mut deletions: Vec<LineChange> = Vec::new();
        let mut additions: Vec<LineChange> = Vec::new();

        'outer: for (original_index, original_line) in original_string.lines().enumerate() {
            for (new_index, new_line) in new_string.lines().enumerate().skip(begin_index) {
                if original_line == new_line {
                    begin_index = new_index;

                    if let Some(last_item) = lcs_list.last() {
                        if last_item.original_line + last_item.length - 1 == original_index
                            && last_item.new_line + last_item.length - 1 == new_index
                        {
                            lcs_list.last_mut().unwrap().length += 1;
                            continue 'outer;
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

                    let mut last_two_lcs = lcs_list.iter().rev().take(2);
                    last_two_lcs.next();
                    let before_last_lcs = last_two_lcs.next().unwrap_or(&LCSItem {
                        original_line: 0,
                        new_line: 0,
                        length: 1,
                    });

                    let addition_length =
                        new_index - (before_last_lcs.new_line + before_last_lcs.length - 1);

                    if addition_length != 0 {
                        additions.push(LineChange {
                            next_lcs: lcs_list.len() - 1,
                            length: addition_length,
                            change_type: ChangeType::Added,
                        });
                    }

                    continue 'outer;
                }
            }

            if let Some(last_change) = deletions.last_mut() {
                if last_change.next_lcs == lcs_list.len() {
                    last_change.length += 1;
                } else {
                    deletions.push(LineChange {
                        next_lcs: lcs_list.len(),
                        length: 1,
                        change_type: ChangeType::Removed,
                    })
                }
            } else {
                deletions.push(LineChange {
                    next_lcs: lcs_list.len(),
                    length: 1,
                    change_type: ChangeType::Removed,
                })
            }
        }

        if let Some(last_lcs) = lcs_list.last() {
            let last_lcs_line = last_lcs.new_line + last_lcs.length - 1;
            let new_lines = new_string.lines().count();

            if last_lcs_line != new_lines {
                additions.push(LineChange {
                    next_lcs: lcs_list.len(),
                    length: new_lines - last_lcs_line,
                    change_type: ChangeType::Added,
                })
            }
        }

        let mut line_changes = [deletions, additions].concat();
        line_changes.sort();

        LinesDiff {
            line_changes,
            lcs_list,
            old_text: original_string,
            new_text: new_string,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::*;

    type TestPair = (
        (&'static str, &'static str),
        &'static [LineChange],
        &'static [LCSItem],
    );

    const TEST_PAIRS: &'static [TestPair] = &[(
        (
            &"first line\nsecond line\nthird line",
            &"new first line\nsecond line\nsomething changed here\nlast line",
        ),
        &[
            LineChange {
                next_lcs: 0,
                length: 1,
                change_type: ChangeType::Removed,
            },
            LineChange {
                next_lcs: 0,
                length: 1,
                change_type: ChangeType::Added,
            },
            LineChange {
                next_lcs: 1,
                length: 1,
                change_type: ChangeType::Removed,
            },
            LineChange {
                next_lcs: 1,
                length: 2,
                change_type: ChangeType::Added,
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
            assert_eq!(diff.line_changes.as_slice(), pair.1);
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
