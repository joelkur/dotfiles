-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    file_ignore_patterns = { ".git", "node%_modules" },
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
  pickers = {
    find_files = {
      hidden = true,
    },
    live_grep = {
      additional_args = function()
        return { '--hidden', '--glob', '!**/.git/*' }
      end
    },
    grep_string = {
      additional_args = function()
        return { '--hidden', '--glob', '!**/.git/*' }
      end
    },
  },
  extensions = {
    file_browser = {
      hidden = { file_browser = true, folder_browser = true }
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'file_browser')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer' })

vim.keymap.set("n", "<C-H>", "<C-W>h", { desc = "Switch to left window" })
vim.keymap.set("n", "<C-J>", "<C-W>j", { desc = "Switch to below window" })
vim.keymap.set("n", "<C-K>", "<C-W>k", { desc = "Switch to above window" })
vim.keymap.set("n", "<C-L>", "<C-W>l", { desc = "Switch to right window" })

vim.keymap.set('n', '<leader>gf', require('telescope.builtin').git_files, { desc = 'Search [G]it [F]iles' })
vim.keymap.set('n', '<leader>gg', ":LazyGit<CR>", { desc = 'Start Lazy[G]it' })
vim.keymap.set('n', '<leader>sf', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>ft', ":Telescope file_browser path=%:p:h select_buffer=true<CR>",
  { desc = 'Telescope file browser' })
vim.keymap.set('n', '<leader>fb', ":NvimTreeToggle<CR>",
  { desc = 'Toggle NvimTree' })
