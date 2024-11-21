use std::borrow::Cow;
use std::borrow::Cow::{Borrowed, Owned};
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::validate::MatchingBracketValidator;
use cas3_core::{Cas3VM, evaluate, Expr, expr_parser};

#[derive(Helper, Completer, Hinter, Validator)]
pub struct ReplHelper {
    pub highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    pub validator: MatchingBracketValidator,
    pub colored_prompt: String,
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

pub fn run(
    mut rl: rustyline::Editor<ReplHelper, rustyline::history::FileHistory>,
    mut ctx: Cas3VM,
) -> rustyline::Result<()> {
    let mut i = 1;

    loop {
        let prompt = format!("(In {}) := ", i);
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{prompt}\x1b[0m");

        let line = rl.readline(&prompt); // read
        match line {
            Ok(l) => {
                rl.add_history_entry(l.as_str()).unwrap(); // history
                // saving every line (even if slow, just until its more stable)
                rl.save_history("history.txt").unwrap();

                let exs = expr_parser::expressions(&l);

                match exs {
                    Ok(exprs) => {
                        for expr in exprs {
                            let mut stack = Expr::List(vec![]);
                            let res = evaluate(&mut stack, &mut ctx, &expr);
                            let in_i =
                                expr_parser::Expr(format!("(setd (In {i}) {})", expr).as_str())
                                    .unwrap();
                            evaluate(&mut stack, &mut ctx, &in_i);
                            let out_i =
                                expr_parser::Expr(format!("(set (Out {i}) {})", res).as_str())
                                    .unwrap();
                            evaluate(&mut stack, &mut ctx, &out_i);

                            println!("\x1B[1m(Out {i}) = {}\x1B[0m", res);

                            i += 1;
                        }
                    }

                    Err(err) => println!("Failed to parse: {}", err),
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    } // loop
    Ok(())
}