-- eink.lua - A Neovim theme optimized for e-ink displays
-- Author: Mike Clarke <clarkema@clarkema.org>
-- Date: 2025-04-13

local M = {}

function M.setup()
  -- Clear existing highlights
  vim.cmd('highlight clear')

  if vim.fn.exists('syntax_on') then
    vim.cmd('syntax reset')
  end

  vim.g.colors_name = 'eink'

  -- Define colors
  local colors = {
    bg = 'NONE', -- Use terminal background (white)
    fg = 'NONE', -- Use terminal foreground (black)
    light_gray = '#aaaaaa',
    mid_gray = '#777777',
    dark_gray = '#444444',
    black = '#000000',
    white = '#ffffff',
  }

  -- Define highlight groups
  local groups = {
    -- Editor UI
    Normal = { fg = colors.fg, bg = colors.bg },
    LineNr = { fg = colors.mid_gray },
    CursorLineNr = { fg = colors.black, bold = true },
    Cursor = { fg = colors.white, bg = colors.black },
    CursorLine = { bg = colors.light_gray },
    CursorColumn = { bg = colors.light_gray },
    ColorColumn = { bg = colors.light_gray },
    SignColumn = { fg = colors.mid_gray, bg = colors.bg },
    VertSplit = { fg = colors.mid_gray },
    Folded = { fg = colors.mid_gray, italic = true },
    FoldColumn = { fg = colors.mid_gray },
    MatchParen = { bg = colors.mid_gray },
    NonText = { fg = colors.mid_gray },
    SpecialKey = { fg = colors.mid_gray },
    Visual = { bg = colors.mid_gray },
    VisualNOS = { bg = colors.mid_gray },
    Search = { fg = colors.black, bg = colors.light_gray, bold = true },
    IncSearch = { fg = colors.black, bg = colors.light_gray, bold = true },
    Pmenu = { fg = colors.black, bg = colors.light_gray },
    PmenuSel = { fg = colors.white, bg = colors.black },
    PmenuSbar = { bg = colors.light_gray },
    PmenuThumb = { bg = colors.mid_gray },
    StatusLine = { fg = colors.black, bg = colors.light_gray, bold = true },
    StatusLineNC = { fg = colors.mid_gray, bg = colors.light_gray },
    WildMenu = { fg = colors.black, bg = colors.light_gray, bold = true },
    TabLine = { fg = colors.mid_gray, bg = colors.light_gray },
    TabLineFill = { fg = colors.mid_gray, bg = colors.light_gray },
    TabLineSel = { fg = colors.black, bg = colors.bg, bold = true },
    Title = { fg = colors.black, bold = true },
    Directory = { fg = colors.black, bold = true },

    -- Syntax highlighting
    Comment = { fg = colors.mid_gray, italic = true },
    Constant = { fg = colors.black },
    String = { fg = colors.dark_gray },
    Character = { fg = colors.dark_gray },
    Number = { fg = colors.black, bold = true },
    Boolean = { fg = colors.black, bold = true },
    Float = { fg = colors.black, bold = true },
    Identifier = { fg = colors.black },
    Function = { fg = colors.black, bold = true },
    Statement = { fg = colors.black, bold = true },
    Conditional = { fg = colors.black, bold = true },
    Repeat = { fg = colors.black, bold = true },
    Label = { fg = colors.black, bold = true },
    Operator = { fg = colors.black },
    Keyword = { fg = colors.black, bold = true },
    Exception = { fg = colors.black, bold = true },
    PreProc = { fg = colors.black },
    Include = { fg = colors.black, bold = true },
    Define = { fg = colors.black },
    Macro = { fg = colors.black },
    PreCondit = { fg = colors.black },
    Type = { fg = colors.black, bold = true },
    StorageClass = { fg = colors.black, bold = true },
    Structure = { fg = colors.black, bold = true },
    Typedef = { fg = colors.black, bold = true },
    Special = { fg = colors.black },
    SpecialChar = { fg = colors.black },
    Tag = { fg = colors.black },
    Delimiter = { fg = colors.black },
    SpecialComment = { fg = colors.mid_gray, italic = true },
    Debug = { fg = colors.black },
    Underlined = { fg = colors.black, underline = true },
    Error = { fg = colors.black, bg = colors.light_gray, bold = true },
    Todo = { fg = colors.black, bg = colors.light_gray, bold = true },

    -- Diagnostics
    DiagnosticError = { fg = colors.black, bold = true },
    DiagnosticWarn = { fg = colors.dark_gray, bold = true },
    DiagnosticInfo = { fg = colors.mid_gray },
    DiagnosticHint = { fg = colors.mid_gray, italic = true },

    -- Git
    DiffAdd = { bg = colors.light_gray },
    DiffChange = { bg = colors.light_gray },
    DiffDelete = { bg = colors.light_gray, fg = colors.mid_gray },
    DiffText = { bg = colors.mid_gray },

    -- LSP
    LspReferenceText = { bg = colors.light_gray },
    LspReferenceRead = { bg = colors.light_gray },
    LspReferenceWrite = { bg = colors.light_gray },
  }

  -- Apply highlights
  for group, styles in pairs(groups) do
    vim.api.nvim_set_hl(0, group, styles)
  end

  -- Terminal colors
  vim.g.terminal_color_0 = colors.black
  vim.g.terminal_color_7 = colors.white
  vim.g.terminal_color_8 = colors.mid_gray
  vim.g.terminal_color_15 = colors.white
end

return M
