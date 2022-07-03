set clipboard=unnamedplus
set tabstop=2
set shiftwidth=2
set expandtab
set number

call plug#begin()
Plug 'neovim/nvim-lspconfig'
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'saadparwaiz1/cmp_luasnip'
Plug 'L3MON4D3/LuaSnip' 
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
call plug#end()

colorscheme tokyonight

lua require('init')
