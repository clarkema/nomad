local ls = require("luasnip")

ls.setup {
    history = true,

    update_events = "TextChanged,TextChangedI",
}
vim.keymap.set({"i"}, "<C-K>", function() ls.expand() end, {silent = true})
vim.keymap.set({"i", "s"}, "<C-L>", function() ls.jump( 1) end, {silent = true})
vim.keymap.set({"i", "s"}, "<C-J>", function() ls.jump(-1) end, {silent = true})

vim.keymap.set({"i", "s"}, "<C-E>", function()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end, {silent = true})

--vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.nomad/nvim-vimplug/LuaSnip/plugin/luasnip.lua<CR>")
vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/lua/my-luasnip.lua<CR>")

local s = ls.snippet
local t = ls.text_node
local c = ls.choice_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

ls.add_snippets("all", {
    s("todo", {
        c(1, {
            t "TODO(clarkema): ",
            t "FIXME(clarkema): ",
            t "TODONT(clarkema): ",
            t "XXX(clarkema): ",
        }),
    }),
})

ls.add_snippets("python", {
    ls.parser.parse_snippet("dbg",
        "print(f'{$0 = }')"
    ),

    ls.parser.parse_snippet("open",
        "with open($1) as f:\n\t$0"
    ),
    
    ls.parser.parse_snippet("try",
        
        'try:\n\t$0\nexcept Exception as e:\n\t'
    ),

    ls.parser.parse_snippet("main",
        'def main() -> None:\n\t$0\n\nif __name__ == "__main__":\n\tmain()'
    ),

    ls.parser.parse_snippet("from",
        'from $1 import $0'
    ),
})
