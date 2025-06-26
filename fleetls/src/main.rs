use fleetls_lib::tower_lsp_server::{LspService, Server};
use fleetls_lib::{Backend, Spawner};
use std::env::args;

pub struct TokioSpawner;

impl Spawner for TokioSpawner {
    fn spawn<F>(&self, fut: F)
    where
        F: std::future::Future<Output = ()> + Send + 'static,
    {
        tokio::spawn(fut);
    }
}

#[tokio::main]
async fn main() {
    if args().any(|arg| arg == "--stdio") {
        let (service, loopback_socket) = LspService::new(|client| Backend {
            client,
            documents: Default::default(),
            spawner: TokioSpawner,
        });

        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

        Server::new(stdin, stdout, loopback_socket)
            .serve(service)
            .await;
    } else {
        #[cfg(not(target_arch = "wasm32"))]
        {
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
                    let (service, loopback_socket) = LspService::new(|client| Backend {
                        client,
                        documents: Default::default(),
                        spawner: TokioSpawner,
                    });

                    let (mut read_half, mut write_half) = tokio::io::split(client_connection);
                    Server::new(&mut read_half, &mut write_half, loopback_socket)
                        .serve(service)
                        .await;
                });
            }
        }
        #[cfg(target_arch = "wasm32")]
        {
            use std::process::exit;

            println!("Networked mode isn't available in the wasm build");
            exit(1);
        }
    }
}
