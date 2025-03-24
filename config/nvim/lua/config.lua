vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)

-- Configure `ruff-lsp`.
-- See: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#ruff_lsp
-- For the default config, along with instructions on how to customize the settings
-- require('lspconfig').ruff.setup {
--     on_attach = function(client, bufnr)
--         local bufopts = { noremap=true, silent=true, buffer=bufnr }
--         vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
--         vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
--     end,
--     init_options = {
--         settings = {
--             -- Any extra CLI arguments for `ruff` go here.
--             args = {},
--         }
--     }
-- }

-- require('lspconfig').pyright.setup {
--   on_attach = on_attach,
--   init_options = {
--     settings = {
--       -- Any extra CLI arguments for `ruff` go here.
--       args = {},
--     }
--   }
-- }
-- 
-- require('lspconfig').elixirls.setup {
-- --cmd = { "/home/clarkema/.nix-profile/bin/elixir-ls" }
--   cmd = { "elixir-ls" }
-- }

-- local builtin = require('telescope.builtin')
-- vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
-- vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
-- vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
-- 
-- local telescope = require("telescope")
-- 
-- telescope.setup({
-- 
-- })

-- require('nvim-treesitter.configs').setup {
--     ensure_installed = { "lua", "vim", "vimdoc", "python", "query", "rust", "haskell", "elixir", "eex", "heex" },
-- 
--     auto_install = false,
-- 
--     highlight = {
--         enable = true,
--     },
-- 
--     incremental_selection = {
--         enable = true,
--         keymaps = {
--             init_selection = "<Leader>ss",
--             node_incremental = "<Leader>si",
--             scope_incremental = "<Leader>sc",
--             node_decremental = "<Leader>sd",
-- 
--         },
--     },
-- }

-- require('my-luasnip')

-- Treat '-' as a word character in CSS and friends, to ensure '*',
-- '<leader>fc' etc. all work as expected
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "css", "scss", "sass" },
  callback = function()
    vim.opt_local.iskeyword:append("-")
  end
})
