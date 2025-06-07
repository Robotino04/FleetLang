#!/usr/bin/env bash

function restart_lsp() {
    for file in "$XDG_RUNTIME_DIR/nvim."*; do
        echo "restarting instance $file"
        nvim --server $file --remote-expr 'execute("source load.vim")' --headless
        nvim --server $file --remote-expr 'execute("LspStop fleetls")' --headless
        nvim --server $file --remote-expr 'execute("LspStart fleetls")' --headless
    done
}

(cargo run --bin fleetls --color=always 2>&1 || (
    echo "\033[31m!!Starting last successful build!!\033[0m" && ./target/debug/fleetls &
    restart_lsp
)) | while IFS= read -r line; do
    echo -e "$line"
    if [[ "$line" == *"Running"* ]]; then
        restart_lsp
    fi
done && echo "FleetLS exited"
