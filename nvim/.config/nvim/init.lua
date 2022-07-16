vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.number = true
vim.o.relativenumber = true
vim.o.background = "dark"

local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<C-H>', '<C-W>h', opts)
vim.api.nvim_set_keymap('n', '<C-J>', '<C-W>j', opts)
vim.api.nvim_set_keymap('n', '<C-K>', '<C-W>k', opts)
vim.api.nvim_set_keymap('n', '<C-L>', '<C-W>l', opts)

require('init')
vim.cmd[[colorscheme gruvbox]]
