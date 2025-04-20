vim.g.mapleader = " "

local keymap = vim.keymap -- for conciseness

keymap.set("i", "kj", "<ESC>", { desc = "Exit insert mode with jk" })
keymap.set("n", "<leader>nh", ":nohl<CR>", { desc = "Clear search highlights" })

keymap.set("n", "<leader>nh", ":nohl<CR>", { desc = "Clear search highlights" })

keymap.set("n", "<M-j>", "<cmd>cnext<CR>", { desc = "Move to next item in quickfix list" })
keymap.set("n", "<M-k>", "<cmd>cprev<CR>", { desc = "Move to previous item in quickfix list" })

