use std::{
    rc::Rc,
    sync::{Arc, Mutex, atomic::AtomicBool},
    time::Duration,
};

use fleetls_lib::{
    Backend, BackgroundThreadState,
    fleet::{
        error_reporting::{ErrorSeverity, Errors},
        infra::{
            insert_c_passes, insert_compile_passes, insert_fix_passes, insert_minimal_pipeline,
        },
        passes::{
            ast_json_dump::{AstJsonDumpPass, AstJsonOutput},
            pass_manager::{CCodeOutput, InputSource, PassError, PassManager},
        },
        tokenizer::FileName,
    },
    tower_lsp_server::{LspService, Server},
};
use futures::stream::TryStreamExt;
use log::{Level, info};
use wasm_bindgen::{JsCast, prelude::wasm_bindgen, prelude::*};
use wasm_bindgen_futures::{spawn_local, stream::JsStream};

#[wasm_bindgen(start)]
pub fn startup() {
    console_error_panic_hook::set_once();
    console_log::init_with_level(Level::Info).expect("error initializing log");
}

#[wasm_bindgen]
pub struct ServerConfig {
    into_server: js_sys::AsyncIterator,
    from_server: web_sys::WritableStream,
}

#[wasm_bindgen]
impl ServerConfig {
    #[wasm_bindgen(constructor)]
    pub fn new(into_server: js_sys::AsyncIterator, from_server: web_sys::WritableStream) -> Self {
        Self {
            into_server,
            from_server,
        }
    }
}

#[wasm_bindgen]
pub fn extract_ast(src: String) -> Option<String> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<AstJsonDumpPass>();

    pm.state.insert(InputSource {
        source: src,
        file_name: FileName(Rc::new("web-ui.fl".to_string())),
    });
    pm.state.insert(Errors::default());

    match pm.run() {
        Ok(()) => Some(pm.state.get::<AstJsonOutput>()?.0.clone()),
        Err(_) => None,
    }
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone, Debug)]
pub struct CompileOutput {
    pub ccode: String,
    pub warnings: String,
}

#[wasm_bindgen]
pub fn compile_to_c(source: String) -> Result<CompileOutput, String> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    insert_fix_passes(&mut pm);
    insert_compile_passes(&mut pm);
    insert_c_passes(&mut pm);

    let file_name = FileName(Rc::new("web-ui.fl".to_string()));

    let documents = vec![(file_name.clone(), source.clone())]
        .into_iter()
        .collect();

    pm.state.insert(InputSource { source, file_name });
    let errors = pm.state.insert(Errors::default());

    let run_result = pm.run();
    let mut errors = errors.get(&pm.state).clone();

    let ccode = match run_result {
        Err(err @ (PassError::CompilerError { .. } | PassError::PassManagerStall { .. })) => Err(
            errors.format_all_errors_and_message("Compilation failed internally", &documents, true)
                + "\n"
                + &err.to_string(),
        ),
        Err(PassError::InvalidInput { .. }) => {
            Err(errors.format_all_errors_and_message("Program has errors", &documents, true))
        }
        Ok(()) => Ok(pm
            .state
            .get::<CCodeOutput>()
            .ok_or("Failed to generate C Code")?
            .0
            .clone()),
    }?;

    if errors
        .iter()
        .any(|err| err.severity() == ErrorSeverity::Error)
    {
        eprintln!("these errors should have resulted in a PassError::InvalidInput");
        return Err(errors.format_all_errors_and_message(
            "Compilation has errors",
            &documents,
            true,
        ));
    }

    Ok(CompileOutput {
        ccode,
        warnings: if !(errors.is_empty()) {
            errors.format_all_errors_and_message("There are warnings", &documents, true)
        } else {
            String::new()
        },
    })
}

// NOTE: we don't use web_sys::ReadableStream for input here because on the
// browser side we need to use a ReadableByteStreamController to construct it
// and so far only Chromium-based browsers support that functionality.

// NOTE: input needs to be an AsyncIterator<Uint8Array, never, void> specifically
#[wasm_bindgen]
pub async fn serve(config: ServerConfig) -> Result<(), JsValue> {
    info!("server::serve");

    let ServerConfig {
        into_server,
        from_server,
    } = config;

    let input = JsStream::from(into_server);
    let input = input
        .map_ok(|value| {
            value
                .dyn_into::<js_sys::Uint8Array>()
                .expect("could not cast stream item to Uint8Array")
                .to_vec()
        })
        .map_err(|_err| std::io::Error::from(std::io::ErrorKind::Other))
        .into_async_read();

    let output = JsCast::unchecked_into::<wasm_streams::writable::sys::WritableStream>(from_server);
    let output = wasm_streams::WritableStream::from_raw(output);
    let output = output.try_into_async_write().map_err(|err| err.0)?;

    let (service, messages) = LspService::new(|client| {
        let shared_state = Arc::new(Mutex::new(BackgroundThreadState {
            semantic_tokens_refresh: AtomicBool::new(false),
        }));

        let client_clone = client.clone();
        let state_for_task = shared_state.clone();
        spawn_local(async move {
            loop {
                BackgroundThreadState::run_background_thread(&state_for_task, &client_clone).await;
                gloo_timers::future::sleep(Duration::from_millis(100)).await;
            }
        });

        Backend {
            client,
            background_state: shared_state,
            documents: Default::default(),
        }
    });
    web_sys::console::log_1(&"Server setup".into());
    Server::new(input, output, messages).serve(service).await;

    Ok(())
}
