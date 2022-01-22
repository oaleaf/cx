use lalrpop::Configuration;

fn main() {
    Configuration::new()
        .use_cargo_dir_conventions()
        // .emit_comments(true)
        .emit_rerun_directives(true)
        .emit_whitespace(false)
        .always_use_colors()
        .process()
        .unwrap();
}
