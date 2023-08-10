use std::collections::HashMap;
use std::{
    ffi, fmt, fs,
    io::{self, Read},
    path,
};

use clap::error::ErrorKind;
use clap::{CommandFactory, Parser};

pub enum OutputFormat {
    Normal,
    Brief,
}

const DEFAULT_OUTPUT_FORMAT: OutputFormat = OutputFormat::Normal;

enum PathType {
    File(fs::File),
    Dir(fs::ReadDir),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum BlankPathType {
    File,
    Dir,
    SymLink,
}

impl fmt::Display for BlankPathType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::File => "file",
                Self::Dir => "directory",
                Self::SymLink => "symlink",
            }
        )
    }
}

impl PartialEq for PathType {
    fn eq(&self, other: &Self) -> bool {
        fn to_bool(input: &PathType) -> bool {
            match input {
                PathType::File(_) => false,
                PathType::Dir(_) => true,
            }
        }

        to_bool(self) == to_bool(other)
    }
}

#[derive(clap::Args)]
#[group(multiple = false)]
struct OutputFormatOptions {
    /// output a normal diff (the default)
    #[arg(long)]
    normal: bool,

    /// report only when files differ
    #[arg(long, short = 'q')]
    brief: bool,
}

#[derive(Parser)]
#[command(author, version, about)]
pub struct Args {
    #[command(flatten)]
    output_format: OutputFormatOptions,

    /// The original file
    original: String,
    /// The edited file
    new: String,

    /// report when two files are the same
    #[arg(long, short = 's')]
    report_identical_files: bool,
}

mod utils {
    use std::cmp::Ordering;
    use std::fmt;

    use crate::{Args, OutputFormat};

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
        pub fn output_format<S>(
            &self,
            format: &OutputFormat,
            original_path: S,
            new_path: S,
            args: &Args,
        ) -> String
        where
            S: ToString,
        {
            let mut output = String::new();

            let original_path = original_path.to_string();
            let new_path = new_path.to_string();

            match format {
                OutputFormat::Normal => {
                    let mut skip_next = false;
                    for (index, change) in self.changes.iter().enumerate() {
                        if skip_next {
                            skip_next = false;
                            continue;
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

                        if let Some(next_change) = self.changes.get(index + 1) {
                            if change.next_subsequence == next_change.next_subsequence {
                                output.push_str(&format!(
                                    "{}{}c{}{}\n{}---\n{}",
                                    old_line,
                                    if change.length != 1 {
                                        format!(",{}", old_line + change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    new_line,
                                    if next_change.length != 1 {
                                        format!(",{}", new_line + next_change.length - 1)
                                    } else {
                                        String::new()
                                    },
                                    prepend_to_lines(
                                        &self.old_text,
                                        "< ",
                                        old_line - 1,
                                        change.length
                                    ),
                                    prepend_to_lines(
                                        &self.new_text,
                                        "> ",
                                        new_line - 1,
                                        next_change.length
                                    )
                                ));
                                skip_next = true;
                                continue;
                            }
                        }

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
                                    prepend_to_lines(
                                        &self.new_text,
                                        "> ",
                                        new_line - 1,
                                        change.length
                                    )
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
                                    prepend_to_lines(
                                        &self.old_text,
                                        "< ",
                                        old_line - 1,
                                        change.length
                                    )
                                )
                            }
                        })
                    }
                }
                OutputFormat::Brief => {
                    if !self.changes.is_empty() {
                        output.push_str(&format!(
                            "Files {} and {} differ\n",
                            original_path, new_path
                        ))
                    }
                }
            }

            if args.report_identical_files && self.changes.is_empty() {
                output.push_str(&format!(
                    "Files {} and {} are identical\n",
                    original_path, new_path
                ))
            }

            output
        }
    }

    pub fn diff<S>(original_string: S, new_string: S) -> LineChanges
    where
        S: ToString,
    {
        let original_string = original_string.to_string();
        let new_string = new_string.to_string();

        let mut begin_index = 0;

        let mut subsequence_list: Vec<Subsequence> = Vec::new();
        let mut deletions: Vec<LineChange> = Vec::new();
        let mut additions: Vec<LineChange> = Vec::new();

        'outer: for (original_index, original_line) in original_string.lines().enumerate() {
            for (new_index, new_line) in new_string.lines().enumerate().skip(begin_index) {
                if original_line == new_line {
                    begin_index = new_index + 1;

                    if let Some(last_item) = subsequence_list.last() {
                        if last_item.original_line + last_item.length - 1 == original_index
                            && last_item.new_line + last_item.length - 1 == new_index
                        {
                            subsequence_list.last_mut().unwrap().length += 1;
                            continue 'outer;
                        }
                    }

                    subsequence_list.push(Subsequence {
                        original_line: original_index + 1,
                        new_line: new_index + 1,
                        length: 1,
                    });

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
                    continue;
                }
            }

            deletions.push(LineChange {
                next_subsequence: subsequence_list.len(),
                length: 1,
                change_type: ChangeType::Removed,
            })
        }

        let new_lines_count = new_string.lines().count();

        if let Some(last_subsequence) = subsequence_list.last() {
            let last_subsequence_line = last_subsequence.new_line + last_subsequence.length - 1;

            if last_subsequence_line != new_lines_count {
                additions.push(LineChange {
                    next_subsequence: subsequence_list.len(),
                    length: new_lines_count - last_subsequence_line,
                    change_type: ChangeType::Added,
                })
            }
        } else {
            additions.push(LineChange {
                next_subsequence: 0,
                length: new_lines_count,
                change_type: ChangeType::Added,
            })
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

    fn prepend_to_lines<S, P>(input: S, prefix: P, skip: usize, length: usize) -> String
    where
        S: ToString,
        P: ToString,
    {
        let prefix = prefix.to_string();

        input
            .to_string()
            .lines()
            .skip(skip)
            .take(length)
            .map(|line_text| format!("{}{}\n", prefix, line_text))
            .collect::<String>()
    }
}

fn parse_path<S>(path: S) -> Result<(path::PathBuf, PathType), io::Error>
where
    S: ToString,
{
    let path = path.to_string();

    let metadata = fs::metadata(&path)?;

    if metadata.is_file() {
        Ok((
            path::PathBuf::from(path.clone()),
            PathType::File(fs::File::open(path)?),
        ))
    } else if metadata.is_dir() {
        Ok((
            path::PathBuf::from(path.clone()),
            PathType::Dir(fs::read_dir(path)?),
        ))
    } else {
        unreachable!()
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

    fn file_error_handler<S>(error: io::Error, filename: S) -> !
    where
        S: ToString,
    {
        custom_file_error_handler(
            error,
            format!(
                "Couldn't open file {}. Check if it exists or if the path supplied is valid\n",
                filename.to_string()
            ),
        )
    }

    fn custom_file_error_handler<S>(error: io::Error, message: S) -> !
    where
        S: ToString,
    {
        let mut cmd = Args::command();
        cmd.error(
            ErrorKind::Io,
            format!(
                "{}\nError message: \"{}\"",
                message.to_string(),
                error.to_string()
            ),
        )
        .exit();
    }

    let output_format_options = &args.output_format;
    let (normal, brief) = (output_format_options.normal, output_format_options.brief);

    let output_format = match (normal, brief) {
        (true, false) => OutputFormat::Normal,
        (false, true) => OutputFormat::Brief,
        (false, false) => DEFAULT_OUTPUT_FORMAT,
        _ => unreachable!(),
    };

    let (original, new) = (parse_path(&args.original), parse_path(&args.new));

    match (original, new) {
        (Ok(original), Ok(new)) => match (original, new) {
            ((dir_path, PathType::Dir(mut dir)), (file_path, PathType::File(file)))
            | ((file_path, PathType::File(file)), (dir_path, PathType::Dir(mut dir))) => {
                if let Some(dir_equivalent) = dir.find_map(|entry| match entry {
                    Ok(entry) => {
                        if entry.file_name() == file_path.file_name().unwrap() {
                            Some((
                                entry.path(),
                                fs::File::open(entry.path()).unwrap_or_else(|err| {
                                    custom_file_error_handler(
                                        err,
                                        format!(
                                            "Couldn't find file {} in directory {}",
                                            file_path.display(),
                                            dir_path.display()
                                        ),
                                    )
                                }),
                            ))
                        } else {
                            None
                        }
                    }
                    Err(err) => custom_file_error_handler(err, String::new()),
                }) {
                    let mut files = [(file_path, file), dir_equivalent];
                    if files[0].0.display().to_string() != args.original {
                        files.reverse()
                    }

                    let mut original_string = String::new();
                    let mut new_string = String::new();

                    files[0]
                        .1
                        .read_to_string(&mut original_string)
                        .unwrap_or_else(|err| file_error_handler(err, files[0].0.display()));
                    files[1]
                        .1
                        .read_to_string(&mut new_string)
                        .unwrap_or_else(|err| file_error_handler(err, files[1].0.display()));

                    let diff = utils::diff(original_string, new_string);

                    print!(
                        "{}",
                        diff.output_format(&output_format, &files[0].0, &files[1].0, &args)
                    )
                } else {
                    println!(
                        "diff: {}: No such file or directory",
                        dir_path.join(file_path.file_name().unwrap()).display()
                    );
                }
            }
            ((_, PathType::Dir(original)), (_, PathType::Dir(new))) => {
                let mut new_hashmap: HashMap<ffi::OsString, fs::DirEntry> = HashMap::new();
                new.for_each(|entry| {
                    let entry = entry.unwrap_or_else(|err| custom_file_error_handler(err, ""));
                    new_hashmap.insert(entry.file_name(), entry);
                });

                for entry in original {
                    let entry = entry.unwrap_or_else(|err| custom_file_error_handler(err, ""));

                    if let Some(other_entry) = new_hashmap.remove(&entry.file_name()) {
                        fn parse_filetype(file_type: fs::FileType) -> BlankPathType {
                            if file_type.is_file() {
                                BlankPathType::File
                            } else if file_type.is_dir() {
                                BlankPathType::Dir
                            } else if file_type.is_symlink() {
                                BlankPathType::SymLink
                            } else {
                                unreachable!()
                            }
                        }

                        let entry_type = parse_filetype(entry.file_type().unwrap_or_else(|err| {
                            custom_file_error_handler(err, format!("Unexpected IO error"))
                        }));

                        let other_entry_type =
                            parse_filetype(other_entry.file_type().unwrap_or_else(|err| {
                                custom_file_error_handler(err, format!("Unexpected IO error"))
                            }));

                        match (entry_type, other_entry_type) {
                            (BlankPathType::File, BlankPathType::File) => {
                                let mut original_string = String::new();
                                let mut new_string = String::new();

                                let mut original_file = fs::File::open(entry.path())
                                    .unwrap_or_else(|err| {
                                        custom_file_error_handler(
                                            err,
                                            format!("Unexpected IO error"),
                                        )
                                    });
                                let mut new_file = fs::File::open(other_entry.path())
                                    .unwrap_or_else(|err| {
                                        custom_file_error_handler(
                                            err,
                                            format!("Unexpected IO error"),
                                        )
                                    });

                                original_file
                                    .read_to_string(&mut original_string)
                                    .unwrap_or_else(|err| {
                                        custom_file_error_handler(
                                            err,
                                            format!("Unexpected IO error"),
                                        )
                                    });
                                new_file
                                    .read_to_string(&mut new_string)
                                    .unwrap_or_else(|err| {
                                        custom_file_error_handler(
                                            err,
                                            format!("Unexpected IO error"),
                                        )
                                    });

                                let diff = utils::diff(original_string, new_string);
                                if !diff.changes.is_empty() {
                                    print!(
                                        "diff {} {}\n{}",
                                        entry.path().display(),
                                        other_entry.path().display(),
                                        diff.output_format(
                                            &output_format,
                                            entry.path().display().to_string(),
                                            other_entry.path().display().to_string(),
                                            &args
                                        )
                                    )
                                }
                            }
                            (BlankPathType::Dir, BlankPathType::Dir) => println!(
                                "Common subdirectories: {} and {}",
                                entry.path().display(),
                                other_entry.path().display()
                            ),
                            (BlankPathType::SymLink, BlankPathType::SymLink) => println!(
                                "Common symlinks: {} and {}",
                                entry.path().display(),
                                other_entry.path().display()
                            ),
                            (_, _) => {
                                println!(
                                    "{} is a {} while {} is a {}",
                                    entry.path().display(),
                                    entry_type,
                                    other_entry.path().display(),
                                    other_entry_type
                                )
                            }
                        }
                    } else {
                        println!(
                            "Only in {}: {}",
                            &args.original,
                            entry.file_name().to_string_lossy()
                        )
                    }
                }

                for entry in new_hashmap {
                    println!("Only in {}: {}", &args.new, entry.0.to_string_lossy())
                }
            }
            ((_, PathType::File(mut original)), (_, PathType::File(mut new))) => {
                let mut original_string = String::new();
                let mut new_string = String::new();

                original
                    .read_to_string(&mut original_string)
                    .unwrap_or_else(|err| {
                        custom_file_error_handler(err, format!("Unexpected IO error"))
                    });
                new.read_to_string(&mut new_string).unwrap_or_else(|err| {
                    custom_file_error_handler(err, format!("Unexpected IO error"))
                });

                let diff = utils::diff(original_string, new_string);

                print!(
                    "{}",
                    diff.output_format(&output_format, &args.original, &args.new, &args)
                )
            }
        },
        (Err(err), Ok(_)) => file_error_handler(err, args.original),
        (Ok(_), Err(err)) => file_error_handler(err, args.new),
        (Err(err_original), Err(err_new)) => {
            let mut cmd = Args::command();
            cmd.error(
                ErrorKind::InvalidValue,
                format!(
                    "Couldn't open files {} and {}. Check if they exist or if the file paths supplied are valid\n\
                    First error message: \"{}\"\nSecond error message \"{}\"",
                    args.original,
                    args.new,
                    err_original,
                    err_new
                ),
            )
            .exit();
        }
    }
}
