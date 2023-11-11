use crate::{config::Cas3Config};
use jupyter::{to_value, value_type::HtmlText, ExecutionRequest, JupyterError, JupyterKernelSockets, JupyterMessage};
use cas3_core::{Cas3VM, Expr};

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
    pub(crate) async fn repl_parse_and_run(&mut self, code: &ExecutionRequest) -> Result<(), ValkyrieError> {
        let file = self.vm.load_snippet(&code.code, &format!("Cell{}", code.execution_count));
        for task in self.vm.execute_script(file).await {
            match task {
                Ok(v) => self.send_value(v, &code.header).await,
                Err(e) => self.sockets.send_executed(JupyterError::custom(format!("Error: {}", e)), &code.header).await,
            }
        }
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
            Expr::List(v) => { self.sockets.send_executed(v.to_string(), parent).await }
        }
    }
}
