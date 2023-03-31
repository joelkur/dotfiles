require("nvim-lsp-installer").setup({})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local nvim_lsp = require("lspconfig")
vim.lsp.set_log_level("error")

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set("n", "<space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "<space>f", vim.lsp.buf.formatting, bufopts)
end

vim.g.markdown_fenced_languages = {
  "ts=typescript",
}

local function merge(t1, t2)
  for k, v in pairs(t2) do
    if (type(v) == "table") and (type(t1[k] or false) == "table") then
      merge(t1[k], t2[k])
    else
      t1[k] = v
    end
  end
  return t1
end

local servers = {
  {
    "denols",
    custom_config = {
      root_dir = nvim_lsp.util.root_pattern("deno.json", "deno.jsonc"),
      single_file_support = false,
    },
  },
  { "tsserver", custom_config = {
    root_dir = nvim_lsp.util.root_pattern("package.json"),
  } },
  {
    "lua_ls",
    custom_config = {
      settings = {
        Lua = {
          runtime = {
            version = "LuaJIT",
          },
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
          },
          telemetry = {
            enable = false,
          },
        },
      },
    },
  },
  "rust_analyzer",
  "html",
  "cssls",
  "svelte",
  {
    "gopls",
    custom_config = {
      root_dir = nvim_lsp.util.root_pattern("go.work", "go.mod"),
      settings = {
        analyses = {
          unusedparams = true,
        },
        staticcheck = true,
      },
    },
  },
  "gdscript",
  "prismals",
  "yamlls",
  "pyright",
  "hls",
  "astro",
  "terraformls",
  "tailwindcss",
  "elixirls",
}

for _, lsp in ipairs(servers) do
  local cfg = {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    },
  }

  local name = type(lsp) == "string" and lsp or lsp[1]

  if type(lsp) == "table" then
    merge(cfg, lsp["custom_config"] or {})
  end
  nvim_lsp[name].setup(cfg)
end

-- local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
-- require("null-ls").setup({
--   sources = {
--     require("null-ls").builtins.formatting.stylua,
--     require("null-ls").builtins.formatting.prettier,
--     require("null-ls").builtins.formatting.black,
--     require("null-ls").builtins.diagnostics.eslint,
--     require("null-ls").builtins.completion.spell,
--   },
--   on_attach = function(client, bufnr)
--     if client.supports_method("textDocument/formatting") then
--       vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
--       vim.api.nvim_create_autocmd("BufWritePre", {
--         group = augroup,
--         buffer = bufnr,
--         callback = function()
--           vim.lsp.buf.format({ bufnr = bufnr })
--         end,
--       })
--     end
--   end,
-- })
