return {
    "nvim-tree/nvim-tree.lua",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
        local nvimtree = require("nvim-tree")

        -- nvim-tree recommends this configuration to disable
        -- netrw.  However, netrw provides nvim's support for
        -- editing files via scp, which I use.
        --vim.g.loaded_netrw = 1
        --vim.g.loaded_netrwPlugin = 1

        nvimtree.setup({
            view = {
                width = 35,
            },

            -- change folder arrow icons
            --renderer = {
                --indent_markers = {
                    --enable = true,
                --},
            --}
            actions = {
                open_file = {
                    window_picker = {
                        enable = false,
                    },
                },
            },
            filters = {
                custom ={ ".DS_Store" },
            },
            git = {
                ignore = false,
            },
        })

        local keymap = vim.keymap

        keymap.set("n", "<leader>ee", "<cmd>NvimTreeFocus<CR>", { desc = "Toggle file explorer" })
        keymap.set("n", "<leader>ef", "<cmd>NvimTreeFindFile<CR>", { desc = "Open file explorer on current file" })
        keymap.set("n", "<leader>ex", "<cmd>NvimTreeClose<CR>", { desc = "Close file explorer" })
        keymap.set("n", "<leader>ec", "<cmd>NvimTreeCollapse<CR>", { desc = "Collapse file explorer" })
        keymap.set("n", "<leader>er", "<cmd>NvimTreeRefresh<CR>", { desc = "Refresh file explorer" })
    end
}
