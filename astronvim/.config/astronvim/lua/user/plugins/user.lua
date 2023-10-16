return {
  -- You can also add new plugins here as well:
  -- Add plugins, the lazy syntax
  -- "andweeb/presence.nvim",
  -- {
  --   "ray-x/lsp_signature.nvim",
  --   event = "BufRead",
  --   config = function()
  --     require("lsp_signature").setup()
  --   end,
  -- },
  {
    "folke/todo-comments.nvim",
    opts = {},
    event = "User AstroFile",
    cmd = { "TodoQuickFix" },
    keys = {
      { "<leader>To", "<cmd>TodoTelescope<cr>", desc = "Open TODOs in telescope" },
    },
  },
  {
    "EdenEast/nightfox.nvim",
  },
  {
    "nvim-neotest/neotest",
    event = "User AstroFile",
    dependencies = {
      "haydenmeade/neotest-jest",
      "jfpedroza/neotest-elixir",
    },
    config = function()
      local neotest_ns = vim.api.nvim_create_namespace "neotest"
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)
      require("neotest").setup {
        -- your neotest config here
        adapters = {
          require "neotest-jest",
          require "neotest-elixir",
        },
      }
    end,
  },
}
