vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.number = true
vim.o.background = "dark"

require("plugins-config")
require("lsp-config")
require("cmp-config")
require("telescope-config")
require("nvim-tree-config")
require("snippets")
require("keymaps-config")

vim.cmd[[colorscheme gruvbox]]
