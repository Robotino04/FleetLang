use fleetls_lib::tower_lsp_server::{LspService, Server};
use fleetls_lib::{Backend, BackgroundThreadState};
use std::env::args;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::signal::unix::{SignalKind, signal};
use tokio::time::sleep;
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

#[tokio::main]
async fn main() {
    if args().any(|arg| arg == "--stdio") {
        let (service, loopback_socket) = LspService::new(|client| {
            let shared_state = Arc::new(Mutex::new(BackgroundThreadState {
                semantic_tokens_refresh: AtomicBool::new(false),
            }));

            let client_clone = client.clone();
            let state_for_task = shared_state.clone();
            tokio::spawn(async move {
                loop {
                    BackgroundThreadState::run_background_thread(&state_for_task, &client_clone)
                        .await;
                    sleep(Duration::from_millis(50)).await;
                }
            });

            Backend {
                client,
                background_state: shared_state,
                documents: Default::default(),
            }
        });

        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

        Server::new(stdin.compat(), stdout.compat_write(), loopback_socket)
            .serve(service)
            .await;
    } else {
        let shutdown_flag = Arc::new(AtomicBool::new(false));
        let shutdown_flag_clone = shutdown_flag.clone();

        tokio::spawn(async move {
            let mut sigterm = signal(SignalKind::terminate()).unwrap();
            let mut sigint = signal(SignalKind::interrupt()).unwrap();

            tokio::select! {
                _ = sigterm.recv() => {
                    eprintln!("Received SIGTERM");
                }
                _ = sigint.recv() => {
                    eprintln!("Received SIGINT");
                }
            }

            shutdown_flag_clone.store(true, Ordering::SeqCst);
        });

        let socket = tokio::net::TcpSocket::new_v4().unwrap();
        socket.set_reuseaddr(true).unwrap();
        socket
            .bind(SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 1234))
            .unwrap();
        let listener = socket.listen(5).unwrap();

        loop {
            if shutdown_flag.load(Ordering::SeqCst) {
                eprintln!("Shutting down main loop");
                break;
            }

            let (client_connection, _client_addr) = match tokio::select! {
                conn = listener.accept() => conn,
                _ = sleep(Duration::from_millis(100)) => continue,
            } {
                Ok(pair) => pair,
                Err(err) => {
                    eprintln!("Error accepting connection: {err}");
                    continue;
                }
            };

            let shutdown_flag = shutdown_flag.clone();
            tokio::spawn(async move {
                let (service, loopback_socket) = LspService::new(|client| {
                    let shared_state = Arc::new(Mutex::new(BackgroundThreadState {
                        semantic_tokens_refresh: AtomicBool::new(false),
                    }));

                    let client_clone = client.clone();
                    let state_for_task = shared_state.clone();
                    tokio::spawn(async move {
                        while !shutdown_flag.load(Ordering::SeqCst) {
                            BackgroundThreadState::run_background_thread(
                                &state_for_task,
                                &client_clone,
                            )
                            .await;
                            sleep(Duration::from_millis(50)).await;
                        }
                        eprintln!("Background thread exiting due to shutdown signal.");
                    });

                    Backend {
                        client,
                        background_state: shared_state,
                        documents: Default::default(),
                    }
                });

                let (read_half, write_half) = tokio::io::split(client_connection);
                let (mut read_half, mut write_half) =
                    (read_half.compat(), write_half.compat_write());

                Server::new(&mut read_half, &mut write_half, loopback_socket)
                    .serve(service)
                    .await;

                eprintln!("Client session closed.");
            });
        }

        eprintln!("LSP shutdown complete.");
    }
}
