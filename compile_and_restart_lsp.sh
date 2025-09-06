#!/usr/bin/env bash

trap "kill -TERM 0" SIGTERM

function restart_lsp() {
    for file in "$XDG_RUNTIME_DIR/nvim."*; do
        echo "restarting instance $file"
        nvim --server $file --remote-expr 'execute("source load.vim")' --headless
        nvim --server $file --remote-expr 'execute("lua vim.lsp.enable(\"fleetls\", false)")' --headless
        nvim --server $file --remote-expr 'execute("lua vim.lsp.enable(\"fleetls\", true)")' --headless
    done
}

(
    (cargo build -p fleetls --bin fleetls --color=always 2>&1 || echo "\033[31m!!Starting last successful build!!\033[0m") && (./target/debug/fleetls tcp -p 1234 &)
    restart_lsp
) | while IFS= read -r line; do
    echo -e "$line"
    if [[ "$line" == *"Running"* ]]; then
        restart_lsp
    fi
done && echo "FleetLS exited"
