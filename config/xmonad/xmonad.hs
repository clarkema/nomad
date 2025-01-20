import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab -- Not required for 0.18 and up

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

import XMonad.Actions.DwmPromote
import XMonad.Actions.SwapPromote

-- import ewmh and ewmhFullscreen combinators for EWMH Desktop compliance
import XMonad.Hooks.EwmhDesktops

-- Xmobar imports
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers


main :: IO ()
main = xmonad
 --    . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)


myConfig = def
    { modMask = mod4Mask    -- Rebind Mod to Super
    , terminal = "kitty"
    , layoutHook = myLayout -- Use custom layouts
    , logHook = masterHistoryHook
    , focusedBorderColor = "#00FF00"
    }
    `additionalKeysP`
    [ ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-b", spawn "kitty")
    , ("M-p", spawn "rofi -show combi -combi-modi 'run,drun'")
    -- See https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Actions-SwapPromote.html
    -- 'DWIM' promotion of the current window into the master position, or
    -- swapping of the current with the most recent master
    -- 'True' ignores non-focused floating windows
    , ("M-<Return>", whenX (swapHybrid True) dwmpromote)
    ]



myLayout = tiled ||| Mirror tiled ||| noBorders Full ||| threeCol
  where
    threeCol = renamed [Replace "ThreeCol"]
             $ spacing 3 $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = renamed [Replace "Tiled"]
          $ smartBorders -- Don't show borders on fullscreen floating windows
                         -- such as vlc / mpv
          $ spacing 3 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
