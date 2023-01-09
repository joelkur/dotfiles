local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
end

return require("packer").startup(function(use)
  use("williamboman/nvim-lsp-installer")
  use("neovim/nvim-lspconfig")
  use("leafgarland/typescript-vim")
  use("maxmellon/vim-jsx-pretty")
  use("nvim-lua/plenary.nvim")
  use("nvim-telescope/telescope.nvim")
  use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
  use("hrsh7th/nvim-cmp")
  use("hrsh7th/cmp-nvim-lsp")
  use("saadparwaiz1/cmp_luasnip")
  use({ "L3MON4D3/LuaSnip", tag = "v1.*" })
  use("NLKNguyen/papercolor-theme")
  use("ellisonleao/gruvbox.nvim")
  use("tpope/vim-surround")
  use("nvim-tree/nvim-web-devicons") -- optional, for file icons
  use("nvim-tree/nvim-tree.lua")
  use("folke/lua-dev.nvim")
  use("kdheepak/lazygit.nvim")
  use("rafamadriz/friendly-snippets")
  use({ "iamcco/coc-tailwindcss", run = "yarn install --frozen-lockfile && yarn run build" })
  use("habamax/vim-godot")
  use("pantharshit00/vim-prisma")
  use({ "romgrk/barbar.nvim", requires = { "kyazdani42/nvim-web-devicons" } })
  use({
    "jose-elias-alvarez/null-ls.nvim",
    requires = { "nvim-lua/plenary.nvim" },
  })
  use({ "virchau13/tree-sitter-astro" })
  use({ "wuelnerdotexe/vim-astro" })

  if packer_bootstrap then
    require("packer").sync()
  end
end)
