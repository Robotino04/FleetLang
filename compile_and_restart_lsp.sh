#!/usr/bin/env bash

cargo run --bin fleetls --color=always 2>&1 | while IFS= read -r line; do
    echo -e "$line"
    if [[ "$line" == *"Running"* ]]; then
        nvim --server $XDG_RUNTIME_DIR/nvim.* --remote-send "<Esc><Esc>:source load.vim | LspStop fleetls<CR>" --headless
        nvim --server $XDG_RUNTIME_DIR/nvim.* --remote-send "<Esc><Esc>:LspStart fleetls<CR>" --headless
        nvim --server $XDG_RUNTIME_DIR/nvim.* --remote-send "<Esc><Esc>:if (&ft=='fleet') | edit | endif<CR>" --headless
    fi
done

