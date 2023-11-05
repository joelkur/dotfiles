import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
 where
  threeCol = ThreeColMid nmaster delta ratio
  tiled = Tall nmaster delta ratio
  nmaster = 1
  ratio = 1 / 2
  delta = 3 / 100

myConfig =
  def
    { modMask = mod4Mask
    , terminal = "kitty"
    , layoutHook = myLayout
    }
    `additionalKeysP` [ ("M-S-b", spawn "brave-browser")
                      ]

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig
