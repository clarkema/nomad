-- vim: set foldlevel=0 foldmethod=marker:

vim.cmd("let g:netrw_liststyle = 3")

local opt = vim.opt

-- Indenting and tabs {{{1

opt.autoindent = true
opt.smartindent = true

opt.shiftwidth = 4
opt.tabstop = 4
opt.expandtab = true
opt.smarttab = true
opt.shiftround = true

-- }}}


--opt.relativenumber = true
--opt.number = true
