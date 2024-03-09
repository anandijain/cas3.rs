use std::path::Path;
use rustyline::config::Configurer;
use rustyline::Editor;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use cas3::{ReplHelper, run};
use cas3_core::{Cas3VM, startup_attrs};

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

    let here = Path::new(env!("CARGO_MANIFEST_DIR"));
    startup_attrs(&mut ctx);
    ctx.run_file(here.join("lang/attrs.sexp"))?;
    ctx.run_file(here.join("lang/startup.sexp"))?;
    ctx.run_file(here.join("lang/calculus.sexp"))?;
    // run_file(&mut ctx, Path::new("lang/systems.sexp"))?;

    run(rl, ctx)?;
    Ok(())
}
