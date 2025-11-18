use std::process::Command;

fn main() {
    if std::env::var("SKIP_BUILD_SCRIPT").as_deref().is_ok_and(|skip| {
       skip == "1" || skip == "true"
    }) {
        return
    }

    let out = Command::new("llvm-config")
                        .arg("--libdir")
                        .arg("--libs")
                        .output()
                        .unwrap()
                        .stdout;
    let out = String::from_utf8(out).unwrap();

    let mut split = out.lines();
    let libdir = split.next().unwrap();
    println!("cargo::rustc-link-search={libdir}");

    let libs = split.next().unwrap();
    for lib in libs.split_whitespace().map_while(|l| l.strip_prefix("-l")) {
        println!("cargo::rustc-link-lib={lib}");
    }
}
