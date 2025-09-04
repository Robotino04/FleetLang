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

vim.lsp.config["fleetls"] = {
    filetypes = { "fleet" },
    root_markers = { '.git' },
    cmd =  vim.lsp.rpc.connect("127.0.0.1", 1234),
    -- cmd = { "sh", "-c", "tee fleetls.in | target/debug/fleetls --stdio > fleetls.out 2> fleetls.err" },
    -- cmd = { "target/debug/fleetls", "--stdio"},
    settings = {

    },
}

vim.lsp.enable("fleetls")

if vim.fn.expand('%:e') == "fl" or vim.fn.expand('%:e')  == 'fleet' then
    vim.cmd[[:setfiletype fleet]]
end

EOF
echo "FleetLS is now loaded"
