return {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
        "hrsh7th/cmp-nvim-lsp",
        { "antosha417/nvim-lsp-file-operations", config = true },
        { "folke/neodev.nvim", opts = {} },
    },
    config = function()
        -- import lspconfig plugin
        local lspconfig = require("lspconfig")

        -- import cmp_nvim_lsp plugin for integration with completion
        local cmp_nvim_lsp = require("cmp_nvim_lsp")

        local keymap = vim.keymap

        vim.api.nvim_create_autocmd("LspAttach", {
            group = vim.api.nvim_create_augroup("UserLspConfig", {}),
            callback = function(ev)
                local opts = { buffer = ev.buf, silent = true }

                opts.desc = "Show LSP references"
                keymap.set("n", "gR", "<cmd>Telescope lsp_references<CR>", opts)

                opts.desc = "Go to declaration"
                keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

                opts.desc = "Go to declaration"
                keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)
            end
        })

        local capabilities = cmp_nvim_lsp.default_capabilities()

        lspconfig["elixirls"].setup {
            capabilities = capabilities,
            cmd = { "elixir-ls" }
        }

        lspconfig["emmet_ls"].setup {
            capabilities = capabilities,
            filetypes = { "html", "typescriptreact", "javascriptreact", "css", "sass", "scss", "less" }
        }

        lspconfig["bashls"].setup {
            capabilities = capabilities
        }

        lspconfig["perlnavigator"].setup { }
    end
}
