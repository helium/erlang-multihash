use std::{
    env,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

fn main() {
    // Directory containing Cargo.toml
    let repo = env::var("CARGO_MANIFEST_DIR").unwrap();
    // Host triple (arch of machine doing to build, not necessarily the arch we're building for)
    let host_triple = env::var("HOST").unwrap();
    // Target triple (arch we're building for, not necessarily the arch we're building on)
    let target_triple = env::var("TARGET").unwrap();
    // debug or release
    let profile = env::var("PROFILE").unwrap();
    // We use target OS to determine if extension is `.so`, `.dll`, or `.dylib`
    let file_name = match env::var("CARGO_CFG_TARGET_OS").unwrap().as_str() {
        "windows" => "libmultihash.dll",
        "macos" | "ios" => "libmultihash.dylib",
        _ => "libmultihash.so",
    };

    // Location of libmultihash
    let mut so_path: PathBuf = [&repo, "_build", "native"].iter().collect();
    if host_triple != target_triple {
        so_path.push(&target_triple);
    }
    so_path.push(&profile);
    so_path.push(&file_name);

    // Create file in `repo` and write the path to the directory of
    // where to find libmultihash
    let so_path_file_path = Path::new(&repo).join("so-path");
    let mut so_path_file = File::create(so_path_file_path).unwrap();
    write!(so_path_file, "{}", so_path.to_str().unwrap()).unwrap();
}
