use std::collections::HashMap;
use std::path::Path;
use rustyline::config::Configurer;
use rustyline::Editor;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use cas3_core::{Cas3VM, ReplHelper, run, startup_attrs};

fn main() -> rustyline::Result<()> {
    let h = ReplHelper {
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let config = rustyline::Config::default();
    let mut rl = Editor::with_config(config)?;
    rl.set_max_history_size(10000).unwrap();
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut ctx = Cas3VM::default();

    startup_attrs(&mut ctx);
    ctx.run_file("lang/attrs.sexp")?;
    ctx.run_file("lang/startup.sexp")?;
    ctx.run_file("lang/calculus.sexp")?;
    // run_file(&mut ctx, Path::new("lang/systems.sexp"))?;

    run(rl, ctx)?;
    Ok(())
}