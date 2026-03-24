use std::process::Command;

fn main() {
    if std::env::var("SKIP_BUILD_SCRIPT").as_deref().is_ok_and(|skip| {
       skip == "1" || skip == "true"
    }) {
        return
    }
    
    let cmd = 
        Command::new("llvm-config")
        .arg("--libdir")
        .arg("--libs")
        .output();

    let Ok(out) = cmd else {        
        if let Ok(llvm_home) = std::env::var("LLVM_HOME") {
            println!("cargo::rustc-link-search={llvm_home}");
        }
        if let Ok(llvm_c) = std::env::var("LLVM_LIB_NAME") {
            println!("cargo::rustc-link-lib={llvm_c}");
        } else {
            println!("cargo::rustc-link-lib=LLVM-C");
        }
        return
    };

    let out = String::from_utf8(out.stdout).unwrap();

    let mut split = out.lines();
    let libdir = split.next().unwrap();
    println!("cargo::rustc-link-search={libdir}");

    let libs = split.next().unwrap();
    for lib in libs.split_whitespace().map_while(|l| l.strip_prefix("-l")) {
        println!("cargo::rustc-link-lib={lib}");
    }
}
