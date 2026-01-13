local wezterm = require ('wezterm')
local config = wezterm.config_builder()

local scrollback = require 'scrollback'
config.scrollback_lines = 50000
-- Font settings
config.font = wezterm.font("SauceCodePro NFM")
config.font_size = 16

config.colors = {
    cursor_bg = "red",
    cursor_border = "red",
}
config.color_scheme = 'Gruvbox Dark (Gogh)'

-- Tab bar
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

-- Muxing
config.leader = { key = "b", mods = "CTRL", timeout_milliseconds = 2000 }
local act = wezterm.action
config.keys = {
    {
        mods = "SHIFT|CTRL",
        key = "Enter",
        action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" },
    },
    {
        mods = "LEADER",
        key = "c",
        action = wezterm.action.SpawnTab "CurrentPaneDomain",
    },
    {
        mods = "LEADER",
        key = "v",
        action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" },
    },
    {
        mods = "LEADER|SHIFT",
        key = '"',
        action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" },
    },
    {
        mods = "LEADER",
        key = 'q',
        action = wezterm.action.PaneSelect,
    },
    {
        mods = "LEADER | SHIFT",
        key = 'q',
        action = wezterm.action.PaneSelect { mode = "SwapWithActive" },
    },
    {
        mods = 'CTRL|SHIFT|ALT',
        key = 'T',
        action = act.PromptInputLine {
            description = 'Enter new name for tab',
            action = wezterm.action_callback(function(window, pane, line)
                -- line will be `nil` if they hit escape without entering anything
                -- An empty string if they just hit enter
                -- Or the actual line of text they wrote
                if line then
                    window:active_tab():set_title(line)
                end
            end),
        },
    },
    {
        -- Disable default fullscreen binding
        key = 'Enter',
        mods = 'ALT',
        action = wezterm.action.DisableDefaultAssignment,
    },
    {
        key = 'F11',
        mods = 'SHIFT|CTRL',
        action = act.ToggleFullScreen,
    },
    {
        key="F3",
        mods="SHIFT|CTRL",
        action=wezterm.action{EmitEvent="trigger-vim-with-scrollback"}
    },
    {
        key = 'K',
        mods = 'CTRL|SHIFT',
        action = act.ClearScrollback 'ScrollbackAndViewport',
  },
}

return config
