-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
--
--  If you want to override the default filetypes that your language server will attach to you can
--  define the property 'filetypes' to the map in question.

local nvim_lsp = require("lspconfig")
return {
  -- clangd = {},
  -- gopls = {},
  marksman = {},
  pyright = {},
  emmet_ls = {},
  -- rust_analyzer = {},
  tsserver = {
    root_dir = nvim_lsp.util.root_pattern("package.json"),
    single_file_support = false,
  },
  denols = {
    root_dir = nvim_lsp.util.root_pattern("deno.json", "deno.jsonc"),
  },
  html = { filetypes = { 'html', 'twig', 'hbs' } },
  elixirls = {},
  tailwindcss = {
    init_options = {
      userLanguages = {
        elixir = "phoenix-heex",
        heex = "phoenix-heex",
        svelte = "html",
        rust = "html",
      },
    },
    handlers = {
      ["tailwindcss/getConfiguration"] = function(_, _, params, _, bufnr, _)
        vim.lsp.buf_notify(bufnr, "tailwindcss/getConfigurationResponse", { _id = params._id })
      end,
    },
    settings = {
      includeLanguages = {
        typescript = "javascript",
        typescriptreact = "javascript",
        ["html-eex"] = "html",
        ["phoenix-heex"] = "html",
        heex = "html",
        eelixir = "html",
        elixir = "html",
        elm = "html",
        svelte = "html",
        rust = "html",
      },
      tailwindCSS = {
        lint = {
          cssConflict = "warning",
          invalidApply = "error",
          invalidConfigPath = "error",
          invalidScreen = "error",
          invalidTailwindDirective = "error",
          invalidVariant = "error",
          recommendedVariantOrder = "warning",
        },
        experimental = {
          classRegex = {
            [[class= "([^"]*)]],
            [[class: "([^"]*)]],
            '~H""".*class="([^"]*)".*"""',
            '~F""".*class="([^"]*)".*"""',
          },
        },
        validate = true,
      },
    },
  },
  -- hls = {
  --   filetypes = { 'haskell', 'lhaskell', 'cabal' }
  -- },

  lua_ls = {
    settings = {
      Lua = {
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
      },
    }
  },
}
