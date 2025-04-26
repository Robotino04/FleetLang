if exists("g:fleetls_loaded")
    echo "FleetLS already loaded"
    finish
endif

let g:fleetls_loaded = 1

au BufEnter,BufRead,BufNewFile *.fl     setfiletype fleet
au BufEnter,BufRead,BufNewFile *.fleet  setfiletype fleet
"au BufEnter,BufRead,BufNewFile *.fl     source highlight.vim
"au BufEnter,BufRead,BufNewFile *.fleet  source highlight.vim


lua << EOF
local lspconfig = require('lspconfig')
local configs = require 'lspconfig.configs'

configs.fleetls = {
    default_config = {
        filetypes = { "fleet" },
        root_dir = lspconfig.util.root_pattern('.git'),
        cmd = vim.lsp.rpc.connect("127.0.0.1", 1234), -- { "target/debug/fleetls" },
        settings = {

        }
    }
}
lspconfig.fleetls.setup {}
EOF
echo "FleetLS is now loaded"
