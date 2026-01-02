return {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
        "hrsh7th/cmp-nvim-lsp",
        { "antosha417/nvim-lsp-file-operations", config = true },
    },
    config = function()
        local cmp_nvim_lsp = require("cmp_nvim_lsp")

        local capabilities = cmp_nvim_lsp.default_capabilities()

        vim.lsp.config("*", {
            capabilities = capabilities,
        })

        vim.lsp.enable("elixirls")
        vim.lsp.enable("lua_ls")
        --lspconfig["emmet_ls"].setup {
            --capabilities = capabilities,
            --filetypes = { "html", "typescriptreact", "javascriptreact", "css", "sass", "scss", "less" }
        --}

        --lspconfig["bashls"].setup {
            --capabilities = capabilities
        --}

        --lspconfig["perlnavigator"].setup { }
    end
}
