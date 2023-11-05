import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (hPutStrLn, spawnPipe)

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
    , layoutHook = spacingWithEdge 5 myLayout
    , borderWidth = 2
    -- , logHook = myXmobar
    }
    `additionalKeysP` [ ("M-S-b", spawn "brave-browser")
                      ]

myXmobarPP :: PP
myXmobarPP = def

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
