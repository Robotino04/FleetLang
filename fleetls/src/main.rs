use fleetls_lib::tower_lsp_server::{LspService, Server};
use fleetls_lib::{Backend, BackgroundThreadState};
use std::env::args;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};
use std::time::Duration;
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
        use std::net::{IpAddr, Ipv4Addr, SocketAddr};

        let socket = tokio::net::TcpSocket::new_v4().unwrap();
        socket.set_reuseaddr(true).unwrap();
        socket
            .bind(SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 1234))
            .unwrap();
        let listener = socket.listen(5).unwrap();

        loop {
            let (client_connection, _client_addr) = listener.accept().await.unwrap();

            tokio::spawn(async move {
                let (service, loopback_socket) = LspService::new(|client| {
                    let shared_state = Arc::new(Mutex::new(BackgroundThreadState {
                        semantic_tokens_refresh: AtomicBool::new(false),
                    }));

                    let client_clone = client.clone();
                    let state_for_task = shared_state.clone();
                    tokio::spawn(async move {
                        loop {
                            BackgroundThreadState::run_background_thread(
                                &state_for_task,
                                &client_clone,
                            )
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

                let (read_half, write_half) = tokio::io::split(client_connection);
                let (mut read_half, mut write_half) =
                    (read_half.compat(), write_half.compat_write());
                Server::new(&mut read_half, &mut write_half, loopback_socket)
                    .serve(service)
                    .await;
            });
        }
    }
}
