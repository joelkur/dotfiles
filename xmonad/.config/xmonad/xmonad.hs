import XMonad

import XMonad.Hooks.DynamicLog (PP (ppCurrent, ppOutput, ppSep, ppTitleSanitize), dynamicLogWithPP, wrap, xmobarBorder, xmobarColor, xmobarProp, xmobarStrip)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
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
    , layoutHook = myLayout
    , -- , logHook = myXmobar
      borderWidth = 2
    }
    `additionalKeysP` [ ("M-S-b", spawn "brave-browser")
                      ]

-- myXmobarPP :: PP
-- myXmobarPP =
--   def
--     { ppSep = magenta " - "
--     , ppTitleSanitize = xmobarStrip
--     , ppCurrent = wrap " " "" . xmobarBorder "Top" "8be9fd" 2
--     }
--  where
--   magenta = xmobarColor "#ff79c6" ""

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . xmobarProp
    $ myConfig
