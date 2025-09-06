use clap_verbosity_flag::InfoLevel;
use fleetls_lib::tower_lsp_server::{self, LspService, Server};
use fleetls_lib::{Backend, BackgroundThreadState};
use log::{error, info, warn};

use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::signal::unix::{SignalKind, signal};
use tokio::time::sleep;
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

use clap::{Parser, Subcommand};

use crate::flush_on_write::ToFlushOnWrite;

pub mod flush_on_write;

fn create_lsp_service(
    shutdown_flag: Arc<AtomicBool>,
) -> (
    tower_lsp_server::LspService<Backend>,
    tower_lsp_server::ClientSocket,
) {
    LspService::new(|client| {
        let shared_state = Arc::new(Mutex::new(BackgroundThreadState {
            semantic_tokens_refresh: AtomicBool::new(false),
        }));

        let client_clone = client.clone();
        let state_for_task = shared_state.clone();

        tokio::spawn(async move {
            loop {
                if shutdown_flag.load(Ordering::SeqCst) {
                    warn!("Background thread exiting due to shutdown signal.");
                    break;
                }

                BackgroundThreadState::run_background_thread(&state_for_task, &client_clone).await;
                sleep(Duration::from_millis(50)).await;
            }
        });

        Backend {
            client,
            background_state: shared_state,
            documents: Default::default(),
        }
    })
}

#[derive(Clone, Copy, Debug, Subcommand)]
enum TransportMode {
    /// Send messages on stdout and receive them on stdin
    Stdio {},

    /// Send messages on a tcp socket
    Tcp {
        #[arg(short, long)]
        port: u16,
    },
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    transport_mode: TransportMode,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity<InfoLevel>,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    env_logger::Builder::new()
        /*
        .format(|fmt, record| {
            let level = record.level();
            let level_style = fmt.default_level_style(level);
            writeln!(
                fmt,
                "{level_style}[{level}]{level_style:#} {}",
                record.args()
            )
        })
        */
        .filter_level(cli.verbosity.into())
        .parse_default_env()
        .init();

    let shutdown_flag = Arc::new(AtomicBool::new(false));

    let shutdown_flag_clone = shutdown_flag.clone();
    tokio::spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).unwrap();
        let mut sigint = signal(SignalKind::interrupt()).unwrap();

        tokio::select! {
            _ = sigterm.recv() => {
                warn!("Received SIGTERM");
            }
            _ = sigint.recv() => {
                warn!("Received SIGINT");
            }
        }

        shutdown_flag_clone.store(true, Ordering::SeqCst);
    });

    match cli.transport_mode {
        TransportMode::Stdio {} => {
            let (service, loopback_socket) = create_lsp_service(shutdown_flag.clone());

            let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

            Server::new(
                stdin.compat(),
                stdout.compat_write().flush_on_write(),
                loopback_socket,
            )
            .serve(service)
            .await;
        }
        TransportMode::Tcp { port } => {
            let socket = tokio::net::TcpSocket::new_v4().unwrap();
            socket.set_reuseaddr(true).unwrap();
            socket
                .bind(SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), port))
                .unwrap();
            let listener = socket.listen(5).unwrap();

            loop {
                if shutdown_flag.load(Ordering::SeqCst) {
                    info!("Shutting down main loop");
                    break;
                }

                let (client_connection, _client_addr) = match tokio::select! {
                    conn = listener.accept() => conn,
                    _ = sleep(Duration::from_millis(100)) => continue,
                } {
                    Ok(ok) => ok,
                    Err(err) => {
                        error!("Error accepting connection: {err}");
                        continue;
                    }
                };

                let shutdown_flag = shutdown_flag.clone();
                tokio::spawn(async move {
                    let (service, loopback_socket) = create_lsp_service(shutdown_flag.clone());

                    let (read_half, write_half) = tokio::io::split(client_connection);
                    let (mut read_half, mut write_half) =
                        (read_half.compat(), write_half.compat_write());

                    Server::new(&mut read_half, &mut write_half, loopback_socket)
                        .serve(service)
                        .await;

                    info!("Client session closed.");
                });
            }
        }
    }

    info!("LSP shutdown complete.");
}
