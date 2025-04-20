vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)

vim.diagnostic.handlers.loclist = {
  show = function(_, _, _, opts)
    -- Generally don't want it to open on every update
    opts.loclist.open = opts.loclist.open or false
    local winid = vim.api.nvim_get_current_win()
    vim.diagnostic.setloclist(opts.loclist)
    vim.api.nvim_set_current_win(winid)
  end
}

vim.diagnostic.config({
    -- or just 'true' to always show regardless of where the cursor is
    virtual_lines = { current_line = true },
    loclist = {
        open = true,
        severity = { min = vim.diagnostic.severity.WARN },
    }
})

-- Don't show line / column numbers in the status bar, to prevent them constantly updating on eink screens
vim.opt.ruler = false
