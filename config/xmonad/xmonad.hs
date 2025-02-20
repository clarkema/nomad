import XMonad

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
--import XMonad.Util.Ungrab -- Not required for 0.18 and up
import XMonad.Util.SpawnOnce

import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize

import XMonad.Layout.ThreeColumns
--import XMonad.Layout.Magnifier
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.CenteredIfSingle

import XMonad.Actions.DwmPromote
import XMonad.Actions.SwapPromote

-- import ewmh and ewmhFullscreen combinators for EWMH Desktop compliance
import XMonad.Hooks.EwmhDesktops

-- Xmobar imports
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

-- apt install cabal-install / cabal install xmonad-dbus
import qualified XMonad.DBus as D
import qualified DBus.Client as DC

main :: IO ()
main = do
    dbus <- D.connect
    D.requestAccess dbus
    xmonad
      -- . ewmhFullscreen
      . ewmh
      -- . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
      . withEasySB (statusBarProp "polybar xmonad" (pure myXmobarPP)) defToggleStrutsKey
      $ (myConfig dbus)
  where
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)


myConfig dbus = def
    { modMask = mod4Mask    -- Rebind Mod to Super
    , terminal = "kitty"
    , layoutHook = myLayout -- Use custom layouts
    --, logHook = masterHistoryHook <> refocusLastLogHook <> dbusHook
    , logHook = dynamicLogWithPP (dbusHook dbus)
    , startupHook = myStartupHook
    , borderWidth = 2
    , focusedBorderColor = colourBlue
    , workspaces = myWorkspaces
    }
    `additionalKeysP`
    [ ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-b", spawn "kitty")
    , ("M-C-<Return>", spawn "kitty")
    , ("M-p", spawn "rofi -show combi -combi-modi 'drun,run'")
    , ("M-<Return>", toggleFocus)
    -- See https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Actions-SwapPromote.html
    -- 'DWIM' promotion of the current window into the master position, or
    -- swapping of the current with the most recent master
    -- 'True' ignores non-focused floating windows
    , ("M-S-<Return>", whenX (swapHybrid True) dwmpromote)
    -- Next / Prev workspace
    , ("M-<Right>", nextWS)
    , ("M-<Left>", prevWS)
    -- Minimise
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)

    -- Add missing keybindings for additional workspaces
    , ("M-0", (windows $ W.greedyView "0"))
    , ("M-S-0", (windows $ W.shift "0"))
    , ("M-<Underscore>", (windows $ W.shift "scratch"))
    -- Toggles
    , ("M-z", sendMessage ToggleLayout)
    , ("M-n", withFocused $ sendMessage . maximizeRestore)
    -- Audio
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioPlay>", spawn "playerctl --player=mpd play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s '+10%'")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s '10%-'")
    ]



myLayout = centerTiled ||| Mirror tiled ||| noBorders Full ||| threeMax ||| grid
  where
    threeMax = renamed [Replace "ThreeMax"]
             $ centeredIfSingle 0.55 1.0
             $ minimize
             $ toggleLayouts (noBorders Full) threeCol

    threeCol = renamed [Replace "ThreeCol"]
             $ smartBorders
             $ spacing 3
             -- $ magnifiercz' 1.3
             $ ThreeColMid nmaster delta ratio
    tiled = renamed [Replace "Tiled"]
          $ minimize
          $ smartBorders -- Don't show borders on fullscreen floating windows
                         -- such as vlc / mpv
          $ spacing 3
          $ toggleLayouts (noBorders Full) (mouseResizableTile { draggerType = BordersDragger }) -- Tall nmaster delta ratio

    centerTiled = renamed [Replace "Center Tiled"]
                $ centeredIfSingle 0.55 1.0
                $ tiled

    grid = maximize Grid

    nmaster = 1
    ratio = 1/2
    delta = 3/100

myStartupHook = do
  -- Start a polkit agent to handle authentication requests. Required by, for
  -- e.g., virt-manager.
  spawnOnce "systemctl start --user plasma-polkit-agent"
  spawnOnce "picom --backend glx --vsync --config /dev/null"
  spawnOnce "nm-applet"
  spawnOnce "gammastep -P -O 3500"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "scratch"]

--dbusHook :: DC.Client -> PP
dbusHook dbus = def
    { ppOutput = D.send dbus
    , ppSep             = pbarColour colourBlue " • "
    , ppCurrent         = (pbarColour colourWhite) . wrap "%{o#44aacc}%{+o} " " %{-o}"
    , ppVisible         = (pbarColour colourWhite) . wrap "%{o#bbbbbb}%{+o} " " %{-o}"
    , ppHidden          = (pbarColour colourWhite) . wrap " " " "
    , ppHiddenNoWindows = (pbarColour colourLowWhite) . wrap " " " "
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = (pbarColour colourWhite) . wrap "[" "]" . ppWindow
    formatUnfocused = (pbarColour colourLowWhite) . wrap "[" "]" . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = (\w -> if null w then "untitled" else w) . shorten 30


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = blue " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" colourBlue 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . blue . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . lowWhite  . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor colourBlue ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

colourBlue = "#44aacc"
colourWhite = "#f8f8f2"
colourLowWhite = "#bbbbbb"

pbarColour :: String -> String -> String
pbarColour colour s = "%{F" ++ colour ++ "}" ++ s ++ "%{F-}"
