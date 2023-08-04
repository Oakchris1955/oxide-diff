use std::fs;

use clap::Parser;

pub enum OutputFormat {
    Normal,
}

const DEFAULT_OUTPUT_FORMAT: OutputFormat = OutputFormat::Normal;

#[derive(clap::Args)]
#[group(multiple = false)]
struct OutputFormatOptions {
    /// Normal output format
    #[arg(long)]
    normal: bool,
}

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[command(flatten)]
    output_format: OutputFormatOptions,

    /// The original file
    original: String,
    /// The edited file
    new: String,
}

mod utils {
    use std::cmp::Ordering;
    use std::fmt;

    use crate::OutputFormat;

    #[derive(PartialEq, Eq, Clone)]
    pub enum ChangeType {
        Removed,
        Added,
    }

    impl fmt::Debug for ChangeType {
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
    pub struct Subsequence {
        pub original_line: usize,
        pub new_line: usize,
        pub length: usize,
    }

    pub struct LineChanges {
        pub changes: Vec<LineChange>,
        pub subsequence_list: Vec<Subsequence>,
        pub old_text: String,
        pub new_text: String,
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct LineChange {
        pub next_subsequence: usize,
        pub length: usize,
        pub change_type: ChangeType,
    }

    impl Ord for LineChange {
        fn cmp(&self, other: &Self) -> Ordering {
            if self.next_subsequence == other.next_subsequence {
                if self.change_type == other.change_type {
                    Ordering::Equal
                } else if self.change_type == ChangeType::Removed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                self.next_subsequence.cmp(&other.next_subsequence)
            }
        }
    }

    impl PartialOrd for LineChange {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(if self.next_subsequence == other.next_subsequence {
                if self.change_type == other.change_type {
                    Ordering::Equal
                } else if self.change_type == ChangeType::Removed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                self.next_subsequence.cmp(&other.next_subsequence)
            })
        }
    }

    impl fmt::Debug for LineChanges {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut output = String::new();

            for change in self.changes.iter() {
                let default_subsequence = Subsequence {
                    original_line: self.old_text.lines().count() + 1,
                    new_line: self.new_text.lines().count() + 1,
                    length: 0,
                };

                let subsequence = self
                    .subsequence_list
                    .get(change.next_subsequence)
                    .unwrap_or(&default_subsequence);

                for index in (0..change.length).rev() {
                    output.push_str(&format!(
                        "{:?}{}\n",
                        change.change_type,
                        (match change.change_type {
                            ChangeType::Added => subsequence.new_line,
                            ChangeType::Removed => subsequence.original_line,
                        }) - index
                            - 1
                    ))
                }
            }

            write!(f, "{}", output.trim_end())
        }
    }

    impl LineChanges {
        pub fn output_format(&self, format: OutputFormat) -> String {
            let mut output = String::new();

            match format {
                OutputFormat::Normal => {
                    let mut skip_next = false;
                    for (index, change) in self.changes.iter().enumerate() {
                        if skip_next {
                            skip_next = false;
                            continue;
                        }
                        if let Some(next_change) = self.changes.get(index + 1) {
                            if change.next_subsequence == next_change.next_subsequence {
                                let begin_lines = if change.next_subsequence == 0 {
                                    (1, 1)
                                } else {
                                    let next_subsequence =
                                        &self.subsequence_list[change.next_subsequence - 1];
                                    (
                                        next_subsequence.original_line + next_subsequence.length,
                                        next_subsequence.new_line + next_subsequence.length,
                                    )
                                };
                                output.push_str(&format!(
                                    "{}{}c{}{}\n{}---\n{}",
                                    begin_lines.0,
                                    if change.length != 1 {
                                        format!(",{}", begin_lines.0 + change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    begin_lines.1,
                                    if next_change.length != 1 {
                                        format!(",{}", begin_lines.1 + next_change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    self.old_text
                                        .lines()
                                        .skip(begin_lines.0 - 1)
                                        .take(change.length)
                                        .map(|line_text| format!("< {}\n", line_text))
                                        .collect::<String>(),
                                    self.new_text
                                        .lines()
                                        .skip(begin_lines.1 - 1)
                                        .take(next_change.length)
                                        .map(|line_text| format!("> {}\n", line_text))
                                        .collect::<String>()
                                ));
                                skip_next = true;
                                continue;
                            }
                        }
                        let (old_line, new_line) = if change.next_subsequence == 0 {
                            (1, 1)
                        } else {
                            let next_subsequence =
                                &self.subsequence_list[change.next_subsequence - 1];
                            (
                                next_subsequence.original_line + next_subsequence.length,
                                next_subsequence.new_line + next_subsequence.length,
                            )
                        };

                        output.push_str(&match change.change_type {
                            ChangeType::Added => {
                                format!(
                                    "{}a{}{}\n{}",
                                    old_line - 1,
                                    new_line,
                                    if change.length != 1 {
                                        format!(",{}", new_line + change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    self.new_text
                                        .lines()
                                        .skip(new_line - 1)
                                        .take(change.length)
                                        .map(|line| format!("> {}\n", line))
                                        .collect::<String>()
                                )
                            }
                            ChangeType::Removed => {
                                format!(
                                    "{}{}d{}\n{}",
                                    old_line,
                                    if change.length != 1 {
                                        format!(",{}", old_line + change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    new_line - 1,
                                    self.old_text
                                        .lines()
                                        .skip(old_line - 1)
                                        .take(change.length)
                                        .map(|line| format!("> {}\n", line))
                                        .collect::<String>()
                                )
                            }
                        })
                    }
                }
            }

            output
        }
    }

    pub fn diff<S>(original_string: S, new_string: S) -> LineChanges
    where
        S: Into<String>,
    {
        let original_string: String = original_string.into();
        let new_string: String = new_string.into();

        let mut begin_index = 0;

        let mut subsequence_list: Vec<Subsequence> = Vec::new();
        let mut deletions: Vec<LineChange> = Vec::new();
        let mut additions: Vec<LineChange> = Vec::new();

        'outer: for (original_index, original_line) in original_string.lines().enumerate() {
            for (new_index, new_line) in new_string.lines().enumerate().skip(begin_index) {
                if original_line == new_line {
                    begin_index = new_index;

                    if let Some(last_item) = subsequence_list.last() {
                        if last_item.original_line + last_item.length - 1 == original_index
                            && last_item.new_line + last_item.length - 1 == new_index
                        {
                            subsequence_list.last_mut().unwrap().length += 1;
                            continue 'outer;
                        } else {
                            subsequence_list.push(Subsequence {
                                original_line: original_index + 1,
                                new_line: new_index + 1,
                                length: 1,
                            })
                        }
                    } else {
                        subsequence_list.push(Subsequence {
                            original_line: original_index + 1,
                            new_line: new_index + 1,
                            length: 1,
                        })
                    }

                    let mut last_two_subsequences = subsequence_list.iter().rev().take(2);
                    last_two_subsequences.next();
                    let before_last_subsequence =
                        last_two_subsequences.next().unwrap_or(&Subsequence {
                            original_line: 0,
                            new_line: 0,
                            length: 1,
                        });

                    let addition_length = new_index
                        - (before_last_subsequence.new_line + before_last_subsequence.length - 1);

                    if addition_length != 0 {
                        additions.push(LineChange {
                            next_subsequence: subsequence_list.len() - 1,
                            length: addition_length,
                            change_type: ChangeType::Added,
                        });
                    }

                    continue 'outer;
                }
            }

            if let Some(last_change) = deletions.last_mut() {
                if last_change.next_subsequence == subsequence_list.len() {
                    last_change.length += 1;
                } else {
                    deletions.push(LineChange {
                        next_subsequence: subsequence_list.len(),
                        length: 1,
                        change_type: ChangeType::Removed,
                    })
                }
            } else {
                deletions.push(LineChange {
                    next_subsequence: subsequence_list.len(),
                    length: 1,
                    change_type: ChangeType::Removed,
                })
            }
        }

        if let Some(last_subsequence) = subsequence_list.last() {
            let last_subsequence_line = last_subsequence.new_line + last_subsequence.length - 1;
            let new_lines = new_string.lines().count();

            if last_subsequence_line != new_lines {
                additions.push(LineChange {
                    next_subsequence: subsequence_list.len(),
                    length: new_lines - last_subsequence_line,
                    change_type: ChangeType::Added,
                })
            }
        }

        let mut changes = [deletions, additions].concat();
        changes.sort();

        LineChanges {
            changes,
            subsequence_list,
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
        &'static [Subsequence],
    );

    const TEST_PAIRS: &'static [TestPair] = &[(
        (
            &"first line\nsecond line\nthird line",
            &"new first line\nsecond line\nsomething changed here\nlast line",
        ),
        &[
            LineChange {
                next_subsequence: 0,
                length: 1,
                change_type: ChangeType::Removed,
            },
            LineChange {
                next_subsequence: 0,
                length: 1,
                change_type: ChangeType::Added,
            },
            LineChange {
                next_subsequence: 1,
                length: 1,
                change_type: ChangeType::Removed,
            },
            LineChange {
                next_subsequence: 1,
                length: 2,
                change_type: ChangeType::Added,
            },
        ],
        &[Subsequence {
            original_line: 2,
            new_line: 2,
            length: 1,
        }],
    )];

    #[test]
    fn check_diff() {
        for pair in TEST_PAIRS {
            let diff = diff(pair.0 .0, pair.0 .1);
            assert_eq!(diff.changes.as_slice(), pair.1);
            assert_eq!(diff.subsequence_list.as_slice(), pair.2);
        }
    }
}

fn main() {
    let args = Args::parse();

    let original_content = fs::read_to_string(args.original)
        .expect("Couldn't open original file. Check if it exists or if the path supplied is valid");
    let new_content = fs::read_to_string(args.new)
        .expect("Couldn't open new file. Check if it exists or if the path supplied is valid");

    let output_format_options = &args.output_format;
    let normal = output_format_options.normal;

    let output_format = match normal {
        true => OutputFormat::Normal,
        _ => DEFAULT_OUTPUT_FORMAT,
    };

    let diff = utils::diff(original_content, new_content);

    print!("{}", diff.output_format(output_format))
}
