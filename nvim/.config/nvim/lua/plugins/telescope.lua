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
