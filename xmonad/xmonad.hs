import XMonad
import XMonad.Actions.CycleWS -- prevWS and nextWS
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders -- smartBorders
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.DecorationMadness
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run -- spawnPipe
import XMonad.Util.Themes -- spawnPipe
import Graphics.X11.Xinerama -- getScreenInfo
import XMonad.Util.EZConfig -- additionalKeys

main = do
  r <- getScreenRes ":0" 0
  xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = desktopConfig
    { terminal = "alacritty"
    , modMask  = mod4Mask
    , layoutHook = desktopLayoutModifiers $ myLayoutHook
    , borderWidth = 3
    }
    `additionalKeys`
    [
        ((0, xK_F12), spawn "firefox")
        -- keys for conky
        , ((mod4Mask, xK_g), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
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

myLayoutHook = tall ||| max ||| simpleFloat ||| mytabbed1
    where
        max = named "Max" $ smartBorders Full
        -- This gives us gaps around Tall, via X.Layout.Spacing
        tall = wrapped $ smartBorders $ Tall 1 (3/100) (1/2)
        mytabbed1 = named "Simple Tabs1" $ wrapped $ tabbed shrinkText tabbedTheme
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

-- Dzen flags
data DF = DF
    { xPosDF       :: Int
    , yPosDF       :: Int
    , widthDF      :: Int
    , heightDF     :: Int
    , alignementDF :: String
    , fgColorDF    :: String
    , bgColorDF    :: String
    , fontDF       :: String
    , eventDF      :: String
    , extrasDF     :: String
    }

-- Create a dzen string with its flags
dzenFlagsToStr :: DF -> String
dzenFlagsToStr df =
    " -dock -x '" ++ show (xPosDF df) ++
    "' -y '" ++ show (yPosDF df) ++
    "' -w '" ++ show (widthDF df) ++
    "' -h '" ++ show (heightDF df) ++
    "' -ta '" ++ alignementDF df ++
    "' -fg '" ++ fgColorDF df ++
    "' -bg '" ++ bgColorDF df ++
    "' -fn '" ++ fontDF df ++
    "' -e '" ++ eventDF df ++
	"' " ++ extrasDF df

dzenTopLeftFlags :: Res -> DF
dzenTopLeftFlags _ = DF
    { xPosDF       = 0
    , yPosDF       = -1 
    , widthDF      = topPanelSepPos
    , heightDF     = panelHeight
    , alignementDF = "l"
    , fgColorDF    = colorWhiteAlt
    , bgColorDF    = colorBlack
    , fontDF       = dzenFont
    , eventDF      = "onstart=lower"
    , extrasDF     = "-p"
}

-- Top left bar logHook
myTopLeftLogHook h hostname = dynamicLogWithPP def
  { ppOutput          = hPutStrLn h
  , ppOrder           = \(ws:_:_:x) -> ws : x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         = dzenBoxStyle blue2BBoxPP
  , ppUrgent          = dzenBoxStyle red2BBoxPP
  , ppVisible         = dzenBoxStyle green2BBoxPP
  , ppHiddenNoWindows = dzenBoxStyle gray2BBoxPP
  , ppHidden          = dzenBoxStyle white2BBoxPP
  {-, ppExtras          = [ myFocusL ]-}
}
-- Launch dzen through the system shell and return a Handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df


-- Dzen box pretty config
data BoxPP = BoxPP
    { bgColorBPP   :: String
    , fgColorBPP   :: String
    , boxColorBPP  :: String
    , leftIconBPP  :: String
    , rightIconBPP :: String
    , boxHeightBPP :: Int
}

dzenBoxStyle :: BoxPP -> String -> String
dzenBoxStyle bpp t =
    "^fg(" ++ boxColorBPP bpp ++
    ")^i(" ++ leftIconBPP bpp  ++
    ")^ib(1)^r(1920x" ++ show (boxHeightBPP bpp) ++
    ")^p(-1920)^fg(" ++ fgColorBPP bpp ++
    ")" ++ t ++
    "^fg(" ++ boxColorBPP bpp ++
    ")^i(" ++ rightIconBPP bpp ++
    ")^fg(" ++ bgColorBPP bpp ++
    ")^r(1920x" ++ show (boxHeightBPP bpp) ++
	")^p(-1920)^fg()^ib(0)"


-- Dzen logger box pretty printing themes
gray2BoxPP :: BoxPP
gray2BoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorGray
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

blue2BoxPP :: BoxPP
blue2BoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlue
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
    { bgColorBPP   = colorGrayAlt
    , fgColorBPP   = colorWhite
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

red2BBoxPP :: BoxPP
red2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorRed
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

blue2BBoxPP :: BoxPP --current workspace
blue2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorBlue
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

green2BBoxPP :: BoxPP
green2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorGreen
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

gray2BBoxPP :: BoxPP
gray2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorGray
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
}


dzenFont       = "xft:Hack:regular:size=14"
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
