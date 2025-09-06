use std::{
    sync::{Arc, Mutex, atomic::AtomicBool},
    time::Duration,
};

use fleetls_lib::{
    Backend, BackgroundThreadState,
    fleet::{
        infra::{
            insert_c_passes, insert_compile_passes, insert_fix_passes, insert_minimal_pipeline,
        },
        passes::pass_manager::{CCodeOutput, Errors, InputSource, PassManager},
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
pub fn compile_to_c(src: String) -> Option<String> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    insert_fix_passes(&mut pm);
    insert_compile_passes(&mut pm);
    insert_c_passes(&mut pm);

    pm.state.insert(InputSource(src));
    pm.state.insert(Errors::default());

    match pm.run() {
        Ok(()) => Some(pm.state.get::<CCodeOutput>()?.0.clone()),
        Err(_) => None,
    }
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
