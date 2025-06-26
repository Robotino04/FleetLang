if exists("g:fleetls_loaded")
    echo "FleetLS already loaded"
    lua vim.lsp.enable("fleetls")
    finish
endif

let g:fleetls_loaded = 1

"au BufEnter,BufRead,BufNewFile *.fl     setfiletype fleet
"au BufEnter,BufRead,BufNewFile *.fleet  setfiletype fleet
"au BufEnter,BufRead,BufNewFile *.fl     source highlight.vim
"au BufEnter,BufRead,BufNewFile *.fleet  source highlight.vim

lua << EOF
vim.filetype.add({
    extension = {
        fleet = "fleet",
        fl = "fleet",
    },
})

local lspconfig = require('lspconfig')
local configs = require 'lspconfig.configs'

local server_config = {
    filetypes = { "fleet" },
    root_dir = lspconfig.util.root_pattern('.git'),
    cmd = --[[vim.lsp.rpc.connect("127.0.0.1", 1234),]] { "target/debug/fleetls", "--stdio" },
    settings = {

    },
}

configs.fleetls = { default_config = server_config }
vim.lsp.config["fleetls"] = server_config
vim.lsp.enable("fleetls")
lspconfig.fleetls.setup {}

if vim.fn.expand('%:e') == "fl" or vim.fn.expand('%:e')  == 'fleet' then
    vim.cmd[[:setfiletype fleet]]
end

EOF
echo "FleetLS is now loaded"
