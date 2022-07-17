local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<C-H>', '<C-W>h', opts)
vim.api.nvim_set_keymap('n', '<C-J>', '<C-W>j', opts)
vim.api.nvim_set_keymap('n', '<C-K>', '<C-W>k', opts)
vim.api.nvim_set_keymap('n', '<C-L>', '<C-W>l', opts)

vim.api.nvim_set_keymap('n', 'LG', ':LazyGit<Enter>', opts)
vim.api.nvim_set_keymap('n', 'TLG', ':Telescope live_grep<Enter>', opts)
vim.api.nvim_set_keymap('n', 'LINF', ':LspInfo<Enter>', opts)
vim.api.nvim_set_keymap('n', 'LINS', ':LspInstall<Enter>', opts)

