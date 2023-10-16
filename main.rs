use clap::Parser;

#[derive(Debug, Parser)]
struct Args {}

fn main() {
    let args = Args::parse();

    // Tracing.
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;
    
    let env = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| "cas3=trace".into());
    let fmt = tracing_subscriber::fmt::layer().pretty();
    tracing_subscriber::registry().with(fmt).with(env).init();

    //  let ctx = Context::new();
    //  ctx.run();


    // anyhow(features = ["backtrace"]) thiserror
}