use std::collections::HashMap;
use std::process::exit;
use std::{
    ffi, fmt, fs,
    io::{self, Read},
    path,
};

use clap::Parser;

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

    /// Represents a subsequence of common lines in both files
    #[derive(PartialEq, Debug)]
    pub struct Subsequence {
        /// Where the subsequence begins in the original file
        pub original_line: usize,
        /// Where the subsequence begins in the new (edited) file
        pub new_line: usize,
        /// The length of the subsequence (in lines)
        pub length: usize,
    }

    /// Represents the changes between two files-strings
    pub struct LineChanges {
        /// A list of all [`LineChange`]s
        pub changes: Vec<LineChange>,
        /// A list of all [`Subsequence`]s found
        pub subsequence_list: Vec<Subsequence>,
        /// The contents of the old (original) file
        pub old_text: String,
        // The contents of the new (edited) file
        pub new_text: String,
    }

    /// Represent a change within the file
    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct LineChange {
        /// The position of the next [`Subsequence`] within the [`LineChanges`] struct
        pub next_subsequence: usize,
        /// The length of the change (in lines)
        pub length: usize,
        /// The type of the change
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
            // Allocate an empty output String
            let mut output = String::new();

            // Convert both paths to Strings
            let original_path = original_path.to_string();
            let new_path = new_path.to_string();

            // Match-clause for output format
            match format {
                // Normal (--normal) [default format]
                OutputFormat::Normal => {
                    // The way skip_next works is that, if it is, the next iteration of the loop will be skipped...
                    let mut skip_next = false;
                    // Iterate through changes
                    for (index, change) in self.changes.iter().enumerate() {
                        // ...by this clause
                        if skip_next {
                            skip_next = false;
                            continue;
                        }

                        // Get the exactly previous matching subsequence for this change (or return the first subsequenc possible if not found)
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

                        // Try getting the next change (if any) in order to display both changes as a line replacement
                        if let Some(next_change) = self.changes.get(index + 1) {
                            // Proceed only if both changes are behind the same subsequence
                            if change.next_subsequence == next_change.next_subsequence {
                                // Push the change string to the output String
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
                                // Make sure to skip the current and the next iteration of the loop
                                skip_next = true;
                                continue;
                            }
                        }

                        // If the current iteration hasn't been skipped by now, push a string to the output String according to the change type
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
                // Brief (--brief, -q)
                OutputFormat::Brief => {
                    // Push a brief message to the output String only if there are any changes to the files
                    if !self.changes.is_empty() {
                        output.push_str(&format!(
                            "Files {} and {} differ\n",
                            original_path, new_path
                        ))
                    }
                }
            }

            // If no changes found and the --report-identical-files (or -s) flag is present, push a brief message to the output String
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
        // Convert both string to String type
        let original_string = original_string.to_string();
        let new_string = new_string.to_string();

        // Set the begin index to 0 (this makes the algorithm A TON faster, check where the begin_index variable is used)
        let mut begin_index = 0;

        // Self-explanatory
        let mut subsequence_list: Vec<Subsequence> = Vec::new();
        let mut deletions: Vec<LineChange> = Vec::new();
        let mut additions: Vec<LineChange> = Vec::new();

        // Loop through each line in original_string
        'outer: for (original_index, original_line) in original_string.lines().enumerate() {
            // Inner loop to loop each line in new_string, beginning at begin_index
            for (new_index, new_line) in new_string.lines().enumerate().skip(begin_index) {
                if original_line == new_line {
                    // If both lines match, set the begin index to current line + 1 more, in order to skip it
                    begin_index = new_index + 1;

                    // If there is a subsequence within the subsequence_list, and it ends exactly before those very lines,
                    // this means that the current lines are part of the subsequence. Increment its length by 1 and skip outer iteration
                    if let Some(last_item) = subsequence_list.last() {
                        if last_item.original_line + last_item.length - 1 == original_index
                            && last_item.new_line + last_item.length - 1 == new_index
                        {
                            subsequence_list.last_mut().unwrap().length += 1;
                            continue 'outer;
                        }
                    }

                    // If not skipped, get the current last subsequence
                    let last_subsequence = subsequence_list.last().unwrap_or(&Subsequence {
                        original_line: 0,
                        new_line: 0,
                        length: 1,
                    });

                    // See if there are any changes (additions)...
                    let addition_length =
                        new_index - (last_subsequence.new_line + last_subsequence.length - 1);

                    // ...by checking if the number of lines from the end of the last subsequence to the current lines aren't 0
                    if addition_length != 0 {
                        // If yes, push them to the addition vector
                        additions.push(LineChange {
                            next_subsequence: subsequence_list.len(),
                            length: addition_length,
                            change_type: ChangeType::Added,
                        });
                    }

                    // Lastly, push a new subsequence, beginning at the current lines
                    subsequence_list.push(Subsequence {
                        original_line: original_index + 1,
                        new_line: new_index + 1,
                        length: 1,
                    });

                    // Skip outer iteration
                    continue 'outer;
                }
            }

            // If outer iteration not skipped...

            // ...and the last deletion isn't followed by a subsequence...
            if let Some(last_change) = deletions.last_mut() {
                if last_change.next_subsequence == subsequence_list.len() {
                    // Increment its length by 1 and skip iteration
                    last_change.length += 1;
                    continue;
                }
            }

            // Otherwise append a new deletion
            deletions.push(LineChange {
                next_subsequence: subsequence_list.len(),
                length: 1,
                change_type: ChangeType::Removed,
            })
        }

        // Once done looping, make sure there aren't any changes at the end of the new file that we missed
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

        // Flatten deletions and additions vectors into a single vector
        let mut changes = [deletions, additions].concat();
        // Sort that vector
        changes.sort();

        LineChanges {
            changes,
            subsequence_list,
            old_text: original_string,
            new_text: new_string,
        }
    }

    /// A shorthand for `input.lines().skip(n).take(l).map(|line| format!("{}{}\n", prefix, line)).collect::<String>()`
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
    // Parse command line arguments
    let args = Args::parse();

    // A function to handle IO errors more efficiently
    fn file_error_handler<S>(error: io::Error, filename: S) -> !
    where
        S: ToString,
    {
        custom_error_handler(
            error,
            format!(
                "Couldn't open file {}. Check if it exists or if the path supplied is valid\n",
                filename.to_string()
            ),
        )
    }

    // A function to handle IO errors more efficiently with a custom error message
    fn custom_error_handler<S>(error: io::Error, message: S) -> !
    where
        S: ToString,
    {
        eprintln!(
            "{}\nError message: \"{}\"",
            message.to_string(),
            error.to_string()
        );
        exit(2)
    }

    // Figure out an output format according to passed arguments
    let output_format_options = &args.output_format;
    let (normal, brief) = (output_format_options.normal, output_format_options.brief);

    let output_format = match (normal, brief) {
        (true, false) => OutputFormat::Normal,
        (false, true) => OutputFormat::Brief,
        (false, false) => DEFAULT_OUTPUT_FORMAT,
        _ => unreachable!(),
    };

    // Parse paths provided into a Dir or a File instance of PathType
    let (original, new) = (parse_path(&args.original), parse_path(&args.new));

    // Check if both paths were parsed without raising any errors
    match (original, new) {
        // If yes, make sure that we aren't comparing a path against itself
        (Ok(original), Ok(new)) => {
            if original
                .0
                .canonicalize()
                .unwrap_or_else(|err| custom_error_handler(err, "Unexpected IO error"))
                != new
                    .0
                    .canonicalize()
                    .unwrap_or_else(|err| custom_error_handler(err, "Unexpected IO error"))
            {
                // If not, check what kind of instance of PathType original and new are
                match (original, new) {
                    // One of the is a Dir, the other is a File
                    ((dir_path, PathType::Dir(mut dir)), (file_path, PathType::File(file)))
                    | ((file_path, PathType::File(file)), (dir_path, PathType::Dir(mut dir))) => {
                        // Find a file within dir with the same filename as the file variable
                        if let Some(dir_equivalent) = dir.find_map(|entry| match entry {
                            Ok(entry) => {
                                if entry.file_name() == file_path.file_name().unwrap() {
                                    Some((
                                        entry.path(),
                                        fs::File::open(entry.path()).unwrap_or_else(|err| {
                                            custom_error_handler(
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
                            Err(err) => custom_error_handler(err, "Unexpected IO error"),
                        }) {
                            // If a match is found, put both files into an array
                            let mut files = [(file_path, file), dir_equivalent];

                            // If-clause to make sure the files are matched in the order they were provided by the user
                            if files[0].0.display().to_string() != args.original {
                                files.reverse()
                            }

                            // Allocate some memory for the file data
                            let mut original_string = String::new();
                            let mut new_string = String::new();

                            // Read from files
                            files[0]
                                .1
                                .read_to_string(&mut original_string)
                                .unwrap_or_else(|err| {
                                    file_error_handler(err, files[0].0.display())
                                });
                            files[1]
                                .1
                                .read_to_string(&mut new_string)
                                .unwrap_or_else(|err| {
                                    file_error_handler(err, files[1].0.display())
                                });

                            // Compare the files
                            let diff = utils::diff(original_string, new_string);

                            // Print the comparison result
                            print!(
                                "{}",
                                diff.output_format(
                                    &output_format,
                                    &files[0].0.display(),
                                    &files[1].0.display(),
                                    &args
                                )
                            );

                            // Return an appropriate status code
                            if diff.changes.is_empty() {
                                exit(0)
                            } else {
                                exit(1)
                            }
                        } else {
                            // If not, print a message to console and exit
                            eprintln!(
                                "diff: {}: No such file or directory",
                                dir_path.join(file_path.file_name().unwrap()).display()
                            );
                            exit(2)
                        }
                    }
                    // Both files are Dirs
                    ((original_path, PathType::Dir(original)), (new_path, PathType::Dir(new))) => {
                        // Convert the new ReadDir iterator into a path-direntry hashmap
                        let mut new_hashmap: HashMap<ffi::OsString, fs::DirEntry> = HashMap::new();
                        new.for_each(|entry| {
                            let entry = entry.unwrap_or_else(|err| {
                                custom_error_handler(err, "Unexpected IO error")
                            });
                            new_hashmap.insert(entry.file_name(), entry);
                        });

                        let mut diff_found = false;

                        // Loop through the original ReadDir
                        for entry in original {
                            // Convert from Result<DirEntry> to DirEntry
                            let entry = entry.unwrap_or_else(|err| {
                                custom_error_handler(err, "Unexpected IO error")
                            });

                            // Try finding a matching DirEntry on the new hashmap, removing the entry if it exists
                            if let Some(other_entry) = new_hashmap.remove(&entry.file_name()) {
                                // Function to convert FileType struct to custom enum
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

                                // Get the types of both entries using the abovementioned function
                                let entry_type =
                                    parse_filetype(entry.file_type().unwrap_or_else(|err| {
                                        custom_error_handler(err, format!("Unexpected IO error"))
                                    }));

                                let other_entry_type =
                                    parse_filetype(other_entry.file_type().unwrap_or_else(|err| {
                                        custom_error_handler(err, format!("Unexpected IO error"))
                                    }));

                                // Match their types
                                match (entry_type, other_entry_type) {
                                    // If both of them are files...
                                    (BlankPathType::File, BlankPathType::File) => {
                                        // ...begin by allocating some memory for the file data
                                        let mut original_string = String::new();
                                        let mut new_string = String::new();

                                        // Open the files...
                                        let mut original_file = fs::File::open(entry.path())
                                            .unwrap_or_else(|err| {
                                                custom_error_handler(
                                                    err,
                                                    format!("Unexpected IO error"),
                                                )
                                            });
                                        let mut new_file = fs::File::open(other_entry.path())
                                            .unwrap_or_else(|err| {
                                                custom_error_handler(
                                                    err,
                                                    format!("Unexpected IO error"),
                                                )
                                            });

                                        // ... and read them
                                        original_file
                                            .read_to_string(&mut original_string)
                                            .unwrap_or_else(|err| {
                                                custom_error_handler(
                                                    err,
                                                    format!("Unexpected IO error"),
                                                )
                                            });
                                        new_file.read_to_string(&mut new_string).unwrap_or_else(
                                            |err| {
                                                custom_error_handler(
                                                    err,
                                                    format!("Unexpected IO error"),
                                                )
                                            },
                                        );

                                        // Compare the files
                                        let diff = utils::diff(original_string, new_string);

                                        // Print a message ONLY IF there are any changes between the 2 files
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
                                            );

                                            // Also, don't forget to set the diff_found variable to true
                                            diff_found = true;
                                        }
                                    }
                                    // If both of them are directories, print a message to console about this
                                    (BlankPathType::Dir, BlankPathType::Dir) => println!(
                                        "Common subdirectories: {} and {}",
                                        entry.path().display(),
                                        other_entry.path().display()
                                    ),
                                    // If both of them are symlinks, print a message to console about this
                                    (BlankPathType::SymLink, BlankPathType::SymLink) => println!(
                                        "Common symlinks: {} and {}",
                                        entry.path().display(),
                                        other_entry.path().display()
                                    ),
                                    // If they are different, print a message saying so.
                                    (_, _) => {
                                        println!(
                                            "{} is a {} while {} is a {}",
                                            entry.path().display(),
                                            entry_type,
                                            other_entry.path().display(),
                                            other_entry_type
                                        );

                                        // Also, don't forget to set the diff_found variable to true
                                        diff_found = true;
                                    }
                                }
                            } else {
                                // If nothing found, print a message saying that the current entry only exist in the parent directory
                                println!(
                                    "Only in {}: {}",
                                    original_path.display(),
                                    entry.file_name().to_string_lossy()
                                );

                                // Also, don't forget to set the diff_found variable to true
                                diff_found = true;
                            }
                        }

                        // Check if the new_hashmap is empty or not and set the diff_found variable accordingly
                        if !new_hashmap.is_empty() {
                            diff_found = true;
                        }

                        // For the remaining entries in the new_hashmap, say that they only exist in the new directory
                        for entry in new_hashmap {
                            println!(
                                "Only in {}: {}",
                                new_path.display(),
                                entry.0.to_string_lossy()
                            )
                        }

                        if diff_found {
                            exit(1)
                        } else {
                            exit(0)
                        }
                    }
                    // If both of them are files, the process is pretty straightforward
                    (
                        (original_path, PathType::File(mut original)),
                        (new_path, PathType::File(mut new)),
                    ) => {
                        // Allocate some memory for the file data
                        let mut original_string = String::new();
                        let mut new_string = String::new();

                        // Read the file contents
                        original
                            .read_to_string(&mut original_string)
                            .unwrap_or_else(|err| {
                                custom_error_handler(err, format!("Unexpected IO error"))
                            });
                        new.read_to_string(&mut new_string).unwrap_or_else(|err| {
                            custom_error_handler(err, format!("Unexpected IO error"))
                        });

                        // Compare them
                        let diff = utils::diff(original_string, new_string);

                        // Print their differences
                        print!(
                            "{}",
                            diff.output_format(
                                &output_format,
                                original_path.display(),
                                new_path.display(),
                                &args
                            )
                        );

                        // Exit with an appropriate status code
                        if diff.changes.is_empty() {
                            exit(0)
                        } else {
                            exit(1)
                        }
                    }
                }
            } else {
                // If we do, exit with exit code 0, since comparing a path against itself doesn't yield any changes
                exit(0)
            }
        }
        // If any one of the arguments passed couldn't be parsed as a path, use the file_error_handler function
        (Err(err), Ok(_)) => file_error_handler(err, args.original),
        (Ok(_), Err(err)) => file_error_handler(err, args.new),
        // If both arguments passed couldn't be parsed, returns a custom error message
        (Err(err_original), Err(err_new)) => {
            eprintln!(
                    "Couldn't open files {} and {}. Check if they exist or if the file paths supplied are valid\n\
                    First error message: \"{}\"\nSecond error message \"{}\"",
                    args.original,
                    args.new,
                    err_original,
                    err_new
                );
            exit(2)
        }
    }

    // Unreachable code. The program should have exited by now.
    // (the compiler flag below is to not trigger the "unreachable code" warning)
    #[allow(unreachable_code)]
    {
        unreachable!(concat!(
            "This should never be reached.",
            "If you are seeing this, please immediately file an issue at Github,",
            "containing the full error message, along with detailed replication steps"
        ))
    }
}
