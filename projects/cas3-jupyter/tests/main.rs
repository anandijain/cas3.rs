#[test]
fn ready() {
    println!("it works!")
}

use std::path::Path;
use valkyrie_interpreter::ValkyrieVM;

#[tokio::test]
async fn tests() {
    debug_wrong("tests/literals.vk").await.unwrap();
    debug_wrong("tests/collection.vk").await.unwrap();
}

async fn debug_wrong<P: AsRef<Path>>(file: P) -> std::io::Result<()> {
    let mut vm = ValkyrieVM::default();
    let file = vm.load_file(file.as_ref().canonicalize()?)?;
    for i in vm.execute_script(file).await {
        match i {
            Ok(o) => {
                println!("{:#?}", o);
            }
            Err(e) => e.as_report().eprint(vm.as_ref())?,
        }
    }
    Ok(())
}
