use crate::{config::Cas3Config};
use jupyter::{ ExecutionRequest,  JupyterKernelSockets, JupyterMessage};
use cas3_core::{Cas3VM, Expr};
use cas3_core::Result;
pub struct CasExecutor {
    pub(crate) vm: Cas3VM,
    pub(crate) sockets: JupyterKernelSockets,
    pub(crate) config: Cas3Config,
}

impl Default for CasExecutor {
    fn default() -> Self {
        CasExecutor { vm: Cas3VM::default(), sockets: Default::default(), config: Cas3Config::default() }
    }
}

impl CasExecutor {
    pub(crate) async fn repl_parse_and_run(&mut self, code: &ExecutionRequest) -> Result<()> {
        // &format!("Cell{}", code.execution_count)
        let file = self.vm.run_script(&code.code)?;
        self.send_value(file, &code.header).await;
        // for task in self.vm.execute_script(file).await {
        //     match task {
        //         Ok(v) => self.send_value(v, &code.header).await,
        //         Err(e) => self.sockets.send_executed(JupyterError::custom(format!("Error: {}", e)), &code.header).await,
        //     }
        // }
        Ok(())
    }

    pub(crate) async fn send_value(&self, value: Expr, parent: &JupyterMessage) {
        match value {
            Expr::Int(v) => {
                self.sockets.send_executed(v.to_string(), parent).await
            }
            Expr::Real(v) => { self.sockets.send_executed(v.to_string(), parent).await }
            Expr::Sym(v) => { self.sockets.send_executed(v.to_string(), parent).await }
            Expr::Str(v) => { self.sockets.send_executed(v.to_string(), parent).await }
            Expr::List(v) => { self.sockets.send_executed(format!("{v:#?}"), parent).await }
        }
    }
}
