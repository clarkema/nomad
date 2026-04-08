local ok, nix = pcall(require, "nix-plugins")
if not ok then nix = nil end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if nix and nix.enabled then
    -- On Nix: lazy.nvim is in the Nix store; add it directly
    vim.opt.rtp:prepend(nix.path .. "/lazy.nvim")
else
    -- On non-Nix: bootstrap lazy.nvim normally
    if not vim.loop.fs_stat(lazypath) then
        vim.fn.system({
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/folke/lazy.nvim.git",
            "--branch=stable",
            lazypath,
        })
    end
    vim.opt.rtp:prepend(lazypath)
end

require("lazy").setup({
    spec = {
      { import = "clarkema.plugins" },
      { import = "clarkema.plugins.lsp" },
    },

    dev = nix and {
        path = nix.path,
        patterns = { "" },
        fallback = false,
    } or {},

    install = {
        missing = not (nix and nix.enabled), -- don't auto-install on Nix
    },

    -- Disabled to avoid nonsense in RO Nix ephemeral systems.  It
    -- doesn't very based on `nix` or `nix-enabled` because that leads
    -- to it being true on other systems, and I never want nagging.
    checker = {
        enabled = false,
    },

    change_detection = {
        enabled = not (nix and nix.enabled),
    },
    -- No need for rocks for now, so disable to avoid the checkhealth warning
    rocks = {
        enabled = false,
    },
})
