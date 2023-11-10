use std::collections::HashMap;
use std::path::Path;
use rustyline::config::Configurer;
use rustyline::Editor;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use cas3::{Context2, ReplHelper, run_file, startup_attrs};

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
    let mut ctx = Context2 {
        vars: HashMap::new(),
    };

    startup_attrs(&mut ctx);
    run_file(&mut ctx, Path::new("lang/attrs.sexp"))?;
    run_file(&mut ctx, Path::new("lang/startup.sexp"))?;
    run_file(&mut ctx, Path::new("lang/calculus.sexp"))?;
    // run_file(&mut ctx, Path::new("lang/systems.sexp"))?;

    run(rl, ctx)?;
    Ok(())
}