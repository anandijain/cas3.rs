use crate::executor::CasExecutor;
use jupyter::{

     ExecutionReply, ExecutionRequest, JupyterConnection, JupyterError, JupyterKernelProtocol, LanguageInfo,
};
use jupyter_derive::include_png32;

pub mod display;

impl JupyterKernelProtocol for CasExecutor {
    fn language_info(&self) -> LanguageInfo {
        let mut info =
            LanguageInfo::new("cas3", "Cas3").with_syntax("schema", "schema").with_version(env!("CARGO_PKG_VERSION"));
        info.png_32 = include_png32!();
        info.png_64 = include_png32!();
        return info;
    }

    fn connected(&mut self, context: JupyterConnection) {
        self.sockets = context.sockets;
    }

    async fn running(&mut self, code: ExecutionRequest) -> ExecutionReply {
        match self.repl_parse_and_run(&code).await {
            Ok(_) => ExecutionReply::new(true),
            Err(e) => {
                self.sockets.send_executed(JupyterError::custom(e.to_string()), &code.header).await;
                ExecutionReply::new(false)
            }
        }
    }

    fn running_time(&self, time: f64) -> String {
        if self.config.running_time { format!("<sub>Elapsed time: {:.2} seconds.</sub>", time) } else { String::new() }
    }
}
