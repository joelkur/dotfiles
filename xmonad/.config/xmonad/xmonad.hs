import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

-- TODO: Currently copied from xmobar config.
-- Merge xmonad and xmobar to be configured in one project
data CColor = Bg | Fg | Green | Yellow | Red | Blue

color :: CColor -> String
color Bg = "#121212"
color Fg = "#d4be98"
color Green = "#a9b665"
color Yellow = "#e78a4e"
color Red = "#ea6962"
color Blue = "#7daea3"

fc :: String -> CColor -> String
fc text c = "<fc=" ++ color c ++ ">" ++ text ++ "</fc>"

myLayout = smartBorders $ spacingWithEdge 10 $ avoidStruts $ tiled ||| Full ||| Grid
 where
  tiled = Tall nmaster delta ratio
  nmaster = 1
  ratio = 1 / 2
  delta = 3 / 100

myConfig =
  def
    { modMask = mod4Mask
    , terminal = "wezterm"
    , layoutHook = myLayout
    , borderWidth = 2
    , startupHook = myStartupHook
    , manageHook = manageHook def <+> manageDocks
    , focusedBorderColor = color Red
    , normalBorderColor = color Fg
    }
    `additionalKeysP` [ ("M-S-b", spawn "librewolf")
                      , ("M-S-l", spawn $ "i3lock -c " ++ tail (color Bg))
                      ]

fmtXmobarTitle :: String -> String
fmtXmobarTitle t
  | length t >= maxLen = shortened
  | otherwise = t
 where
  maxLen = 30
  shortened = (take maxLen t) ++ "... - " ++ (last $ words t)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = (`fc` Red)
    , ppTitle = \t -> fc (fmtXmobarTitle t) Blue
    , ppLayout = \l -> case l of
        "Spacing Tall" -> "default"
        "Spacing Full" -> "full"
        "Spacing Grid" -> "grid"
        _ -> l
    , ppSep = " | "
    }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill ~/wallpapers/wallpaper.png"
  spawnOnce "picom"

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "my-xmobar-exe" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
