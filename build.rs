use std::{env, path::PathBuf, fs::{read_dir, copy}};

fn main() {
    let mut path_std_in = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    path_std_in.push("std");

    let mut path_std_out = PathBuf::from(env::var("OUT_DIR").unwrap());
    path_std_out.pop();
    path_std_out.pop();
    path_std_out.pop();
    path_std_out.push("std");

    println!("cargo:warning=build std dir: {:?}", path_std_in);
    println!("cargo:warning=target std dir: {:?}", path_std_out);

    copy_reverse(path_std_in, path_std_out);
}

fn copy_reverse(from: PathBuf, to: PathBuf) {
    for entry in read_dir(&from).unwrap() {
        let entry = match entry {
            Ok(v) => v,
            Err(..) => continue,
        };

        let entry_path = entry.path();
        let entry_type = entry.file_type().unwrap();
        let mut to: PathBuf = to.clone();
        to.push(entry.file_name());

        if entry_type.is_dir() {
            copy_reverse(entry_path, to);
        } else if entry_type.is_file() {
            match copy(&entry_path, &to) {
                Ok(..) => println!("cargo:warning=Copied file {} to {}", entry_path.display(), to.display()),
                Err(e) => println!("cargo:error=Failed to copy {} to {}: {:?}", entry_path.display(), to.display(), e),
            }
        } else {
            println!("cargo:warning=Skipped {}", entry_path.display())
        }
    }
}