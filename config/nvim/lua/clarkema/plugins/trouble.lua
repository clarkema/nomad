return {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    cmd = "Trouble",
    keys = {
        { "<leader>xx", "<cmd>Trouble diagnostics toggle<CR>", desc = "Toggle trouble list" },
        { "<leader>xw", "<cmd>TroubleToggle workspace_dianostics<CR>", desc = "Toggle trouble workspace diagnostics" },
        { "<leader>xd", "<cmd>TroubleToggle document_dianostics<CR>", desc = "Toggle trouble document diagnostics" },
        { "<leader>xq", "<cmd>Trouble qflist toggle<CR>", desc = "Open trouble quickfix list" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<CR>", desc = "Open trouble location list" },
    },
}
