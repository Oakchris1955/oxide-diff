# Oxide diff utility

A `diff` utility I wrote in ~10 days.

## Background

I am currently in the unfortunate situation of owning a computer that runs Windows 10. This limits me in many aspects, mainly when it comes to programming. This also means that I can't use some utils only available in UNIX-like operating systems. Sure, I can download MINGW (which I have), but I still am not satisfied by how the Windows OS is structured in many ways.

Around 2 weeks ago, I discovered that the Windows doesn't have a `diff` utility included by default. I thought "sure, I can use the MINGW one", but at the same time I also wondered about whether I could make one myself, just for the experience. So, there we are. This took a while to make and sure, it isn't the most pretty-looking thing in the world, but it seems to work pretty good. I haven't benchmarked it yet, but I plan to do so against the UNIX `diff`

## Installation

First thing first, make sure that you have installed [the Rust programming language](https://www.rust-lang.org/tools/install) in your system

Then, simply clone this repository using [Git](https://git-scm.com/downloads), `cd` to the cloned folder and build your own executable using `cargo build` (or use `cargo run` to run the program it once it is compiled. In this case, seperate the arguments from the `cargo run` command using double quotes (`--`)).

## Usage

The program takes 2 parameters, both of which must be valid paths, and compares their contents.

In detail:

- If both paths are directories, the program will compare their files one-to-one
- If one path is a directory and the other is a file, the program will check if the directory contains a file with the same name. If yes, it will compare their contents
- If both paths are files, the program will compare their contents

For more info about program usage, run it with the `--help` flag
(for those asking, no, the program currently doesn't recursively compare directories. It stops at the first directory level)

## Build profiles

There are 3 profiles you should care about:

1) `dev`: The default profile when running `cargo build`
2) `release`: Using this profile increases compilation time, but generates a faster program overall
3) `release-abort`: Inherits from the `release` profile. The only exception is that, when the program panics, it will immediately abort instead of unwinding (check [this](https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html#unwinding-the-stack-or-aborting-in-response-to-a-panic) for more info)
