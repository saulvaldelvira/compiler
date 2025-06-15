use std::process::Command;

fn main() {
    if std::env::var("SKIP_BUILD_SCRIPT").as_deref().is_ok_and(|skip| {
       skip == "1" || skip == "true"
    }) {
        return
    }

    let out = Command::new("llvm-config")
                        .arg("--ldflags")
                        .output()
                        .unwrap()
                        .stdout;
    let out = String::from_utf8(out).unwrap();

    let prefix = out.trim();
    let prefix = prefix.strip_prefix("-L").unwrap();

    /* println!("cargo::rerun-if-changed={prefix}/libLLVM.so"); */
    println!("cargo::rustc-link-search={prefix}");
    println!("cargo::rustc-link-lib=LLVM");
}
