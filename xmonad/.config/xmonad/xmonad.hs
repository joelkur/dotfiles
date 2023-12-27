import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myLayout = smartBorders $ spacingWithEdge 10 $ avoidStruts $ tiled ||| Full
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
    , focusedBorderColor = "#ea6962"
    , normalBorderColor = "#d4be98"
    }
    `additionalKeysP` [ ("M-S-b", spawn "librewolf")
                      , ("M-S-l", spawn "i3lock -c 121212")
                      ]

myXmobarPP :: PP
myXmobarPP = def

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
