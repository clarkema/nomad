require("clarkema.core.options")
require("clarkema.core.keymaps")

-- https://gist.github.com/bnse/a3eb5b9941e6af4582c5406b29d76e05
local cmp_enabled = true
vim.api.nvim_create_user_command("ToggleAutoComplete", function()
    if cmp_enabled then
        require("cmp").setup.buffer({ enabled = false })
        cmp_enabled = false
    else
        require("cmp").setup.buffer({ enabled = true })
        cmp_enabled = true
    end
end, {})
