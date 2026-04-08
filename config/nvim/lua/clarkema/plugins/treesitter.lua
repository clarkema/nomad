local ok, nix = pcall(require, "nix-plugins")
if not ok then nix = nil end

return {
    "nvim-treesitter/nvim-treesitter",
    --branch = 'master',
    --event = { "BufReadPre", "BufNewFile" },
    lazy = false,
    build = ":TSUpdate",
--    dependencies = {
--        "windwp/nvim-ts-autotag",
--    },
    config = function()
        -- import nvim-treesitter plugin
        local treesitter = require("nvim-treesitter")

        -- configure treesitter
        if nix and nix.enabled then
            treesitter.setup({
            install_dir = (nix and nix.enabled) and nix.parsers or {}
            })
        end
    end

}
