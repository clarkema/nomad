import XMonad
import XMonad.Actions.CycleWS -- prevWS and nextWS
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.CopyWindow
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders -- smartBorders
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ThreeColumns
import XMonad.Layout.DecorationMadness
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run -- spawnPipe
import XMonad.Util.Themes -- spawnPipe
import Graphics.X11.Xinerama -- getScreenInfo
import XMonad.Util.EZConfig -- additionalKeys

main = do
  r <- getScreenRes ":0" 0
  xmonad =<< statusBar myBar myPP toggleStrutsKey (fullscreenSupport myConfig)

myConfig = desktopConfig
    { terminal = "alacritty"
    , modMask  = mod4Mask
    , layoutHook = desktopLayoutModifiers $ myLayoutHook
    , borderWidth = 2
    , normalBorderColor = colorBlack
    }
    `additionalKeys`
    [
        ((0, xK_F12), spawn "firefox")
        , ((0, xK_F11), spawn "thunar")
        -- keys for conky
        , ((mod4Mask, xK_g), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
        , ((mod4Mask .|. shiftMask, xK_b), withFocused toggleBorder)
        -- navigation
        , ((mod4Mask, xK_Left), prevWS)
        , ((mod4Mask, xK_Right), nextWS)

        -- tabs
        , ((mod4Mask .|. controlMask, xK_h), sendMessage $ pullGroup L)
        , ((mod4Mask .|. controlMask, xK_l), sendMessage $ pullGroup R)
        , ((mod4Mask .|. controlMask, xK_k), sendMessage $ pullGroup U)
        , ((mod4Mask .|. controlMask, xK_j), sendMessage $ pullGroup D)

        , ((mod4Mask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
        , ((mod4Mask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

        , ((mod4Mask, xK_v), windows copyToAll)
        , ((mod4Mask .|. shiftMask, xK_v), killAllOtherCopies)

        {-, ((mod4Mask .|. controlMask, xK_period), onGroup W.focusUp')-}
        {-, ((mod4Mask .|. controlMask, xK_comma), onGroup W.focusDown')-}
    ]
    `additionalKeysP`
    [
        -- audio keys
        ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioNext>", spawn "mpc next")
        , ("<XF86AudioPrev>", spawn "mpc prev")
    ]


tabbedTheme = (theme smallClean)
    { activeColor = colorGray
    , activeBorderColor = colorGray
    , inactiveColor = colorBlack
    , inactiveBorderColor = colorBlack
    , decoHeight = 35
    , fontName = "xft:Bitstream Vera Sans Mono Nerd Font:size=10"
    }

wrapped l = spacingRaw False (Border 0 10 10 440) True (Border 10 10 10 10) True  $ l

myLayoutHook = tall ||| max ||| Grid ||| simpleFloat ||| mytabbed1 ||| threecol ||| wide ||| focus
    where
        threecol = named "3Mid" $ wrapped $ ThreeColMid 1 (3/100) (1/2)
        max = named "Max" $ smartBorders Full
        -- This gives us gaps around Tall, via X.Layout.Spacing
        tall = named "Std Tall" $ fullscreenFull $ wrapped $ smartBorders $ Tall 1 (3/100) (1/2)
        focus = named "Focus" $ noBorders $ ThreeColMid 1 (3/100) (1/2)
        mytabbed1 = named "Simple Tabs1" $ wrapped $ tabbed shrinkText tabbedTheme
        wide = named "Wide" $ fullscreenFull $ wrapped $ smartBorders $ Mirror$ Tall 1 (3/100) (1/2)
        {-myTabbed2 = avoidStruts $ windowNavigation $ subTabbed $ Grid-}

myBar = "xmobar" 

myPP = xmobarPP
    { ppCurrent = xmobarColor "#44aacc" ""
    , ppHidden  = xmobarColor "#429942" ""
    , ppHiddenNoWindows = xmobarColor "#444444" ""
    , ppTitle = xmobarColor "#44aacc" ""
    }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


data Res = Res { xRes :: Int, yRes :: Int }

colorBlack     = "#020202"
colorBlackAlt  = "#1c1c1c"
colorGray      = "#444444"
colorGrayAlt   = "#101010"
colorGrayAlt2  = "#404040"
colorGrayAlt3  = "#252525"
colorWhite     = "#a9a6af"
colorWhiteAlt  = "#9d9d9d"
colorWhiteAlt2 = "#b5b3b3"
colorWhiteAlt3 = "#707070"
colorMagenta   = "#8e82a2"
colorBlue      = "#44aacc"
colorBlueAlt   = "#3955c4"
colorRed       = "#f7a16e"
colorRedAlt    = "#e0105f"
colorGreen     = "#66ff66"
colorGreenAlt  = "#558965"
boxLeftIcon2   = "/home/lsund/.icons/xbm_icons/subtle/boxleft2-big.xbm"
boxRightIcon   = "/home/lsund/.icons/xbm_icons/subtle/boxright.xbm"
panelHeight    = 24 :: Int
boxHeight      = 22 :: Int
topPanelSepPos = 950 :: Int
botPanelSepPos = 900 :: Int

-- Gets the current resolution given a display and a screen
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
    dpy <- openDisplay d
    r <- liftIO $ getScreenInfo dpy
    closeDisplay dpy
    return Res
        { xRes = fromIntegral $ rect_width $ r !! n
        , yRes = fromIntegral $ rect_height $ r !! n
}
