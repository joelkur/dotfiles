-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- document existing key chains
require('which-key').register({
  ['<leader>c'] = { name = '[C]ode', _ = 'which_key_ignore' },
  ['<leader>d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
  ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
  ['<leader>h'] = { name = 'More git', _ = 'which_key_ignore' },
  ['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' },
  ['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
  ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
  ['<leader>f'] = { name = '[F]iles', _ = 'which_key_ignore' },
})

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

local servers = require 'plugins.lsp.servers'
mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = (servers[server_name] or {}).settings,
      filetypes = (servers[server_name] or {}).filetypes,
      handlers = (servers[server_name] or {}).handlers,
      init_options = (servers[server_name] or {}).init_options,
      root_dir = (servers[server_name] or {}).root_dir,
      single_file_support = (servers[server_name] or {}).single_file_support,
    }
  end
}

---@type HTOpts
vim.g.haskell_tools = {
  ---@type ToolsOpts
  tools = {
    hover = {
      enable = true
    },
  },
  ---@type HaskellLspClientOpts
  hls = {
    ---@param client number The LSP client ID.
    ---@param bufnr number The buffer number
    ---@param ht HaskellTools = require('haskell-tools')
    on_attach = function(client, bufnr, ht)
      on_attach(client, bufnr)

      local nmap = function(keys, func, desc)
        if desc then
          desc = 'Haskell: ' .. desc
        end

        vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
      end

      require('which-key').register({
        ['<leader>H'] = { name = '[H]askell', _ = 'which_key_ignore' },
      })

      local bufnr = vim.api.nvim_get_current_buf()
      -- local def_opts = { noremap = true, silent = true, buffer = bufnr, }
      -- haskell-language-server relies heavily on codeLenses,
      -- so auto-refresh (see advanced configuration) is enabled by default
      nmap('<space>Hc', vim.lsp.codelens.run, "Run code lens")
      nmap('<space>Hh', ht.hoogle.hoogle_signature,
        "Hoogle search for the type signature of the definition under the cursor")
      nmap('<space>He', ht.lsp.buf_eval_all, "Evaluate all code snippets")
      nmap('<leader>Hr', ht.repl.toggle, "Toggle a GHCi repl for the current package")
      nmap('<leader>Hp', function()
        ht.repl.toggle(vim.api.nvim_buf_get_name(0))
      end, "Toggle GHCi repl for the current buffer")
      nmap('<leader>Hq', ht.repl.quit, "Quit GHCi repl")
    end
  },
  ---@type HTDapOpts
  dap = {},
}

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
