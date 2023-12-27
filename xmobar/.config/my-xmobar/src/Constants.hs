module Constants (
    bgAlpha,
    colorBg,
    colorFg,
    colorGreen,
    colorYellow,
    colorRed,
    font,
) where

import Xmobar (Config, defaultConfig)

bgAlpha :: Int
bgAlpha = 205

colorBg :: String
colorBg = "#121212"

colorFg :: String
colorFg = "#d4be98"

colorGreen :: String
colorGreen = "#a9b665"

colorYellow :: String
colorYellow = "#e78a4e"

colorRed :: String
colorRed = "#ea6962"

font :: [Char]
font = "IosevkaTerm Nerd Font 16"
