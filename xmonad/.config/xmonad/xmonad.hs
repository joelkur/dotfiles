import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myLayout = smartBorders $ spacingWithEdge 10 $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myConfig =
  def
    { modMask = mod4Mask,
      terminal = "wezterm",
      layoutHook = myLayout,
      borderWidth = 2,
      startupHook = myStartupHook,
      focusedBorderColor = "#ea6962",
      normalBorderColor = "#d4be98"
    }
    `additionalKeysP` [ ("M-S-b", spawn "librewolf")
                      ]

myXmobarPP :: PP
myXmobarPP = def

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill ~/wallpapers/wallpaper.png"

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
