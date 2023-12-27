module Main (main) where

import Data.List (intercalate)
import Xmobar

-- TODO: At some point merge xmonad and xmobar configs to be in the same project

bgAlpha :: Int
bgAlpha = 205

data Color = Bg | Fg | Green | Yellow | Red | Blue

color :: Color -> String
color Bg = "#121212"
color Fg = "#d4be98"
color Green = "#a9b665"
color Yellow = "#e78a4e"
color Red = "#ea6962"
color Blue = "#7daea3"

fc :: String -> Color -> String
fc text c = "<fc=" ++ color c ++ ">" ++ text ++ "</fc>"

font' :: String
font' = "IosevkaTerm Nerd Font 16"

alignSep' :: String
alignSep' = "}{"

wrapSpaces :: Int -> String -> String
wrapSpaces n s = spaces ++ s ++ spaces
 where
  spaces = intercalate "" $ map (const " ") (take n [1 ..])

applyAlignSep :: String -> String -> String
applyAlignSep l r = l ++ alignSep' ++ r

buildTemplate :: [String] -> [String] -> String
buildTemplate left right = wrapSpaces 2 $ applyAlignSep (section left) (section right)
 where
  section = intercalate " | "

tmplLeft :: [String]
tmplLeft =
  [ "%battery%"
  , "%multicpu%"
  , "%coretemp%"
  , "%memory%"
  , "%dynnetwork%"
  ]

tmplRight :: [String]
tmplRight =
  [ "dnf: " ++ fc "%dnfupdates%" Green
  , "%date%"
  ]

template' :: String
template' = buildTemplate tmplLeft tmplRight

cmdParams'' :: Bool -> String -> Int -> Int -> [String]
cmdParams'' invertColors tmplt low high =
  [ "--template"
  , tmplt
  , "--Low"
  , show low
  , "--High"
  , show high
  , "--low"
  , if invertColors then color Red else color Green
  , "--normal"
  , color Yellow
  , "--high"
  , if invertColors then color Green else color Red
  ]

cmdParams' :: String -> Int -> Int -> [String]
cmdParams' = cmdParams'' True

cmdParams :: String -> Int -> Int -> [String]
cmdParams = cmdParams'' False

data Template = TNetwork | TCpu | TTemp | TMemory | TBattery

tmpl :: Template -> String
tmpl TNetwork = "<dev> <tx>kB/s|<rx>kB/s"
tmpl TCpu = "cpu: <total0>%|<total1>%"
tmpl TTemp = "temp: <core0>°C|<core1>°C"
tmpl TMemory = "ram: <usedratio>%"
tmpl TBattery = "bat: <acstatus>"

batteryExtraParams :: [String]
batteryExtraParams =
  [ "--"
  , "-o"
  , "<left>% (<timeleft>)"
  , "-O"
  , fc "Charging" Blue
  , "-i"
  , fc "Charged" Green
  ]

commands' :: [Runnable]
commands' =
  [ Run $ DynNetwork (cmdParams (tmpl TNetwork) 1000 5000) 50
  , Run $ MultiCpu (cmdParams (tmpl TCpu) 50 85) 50
  , Run $ CoreTemp (cmdParams (tmpl TTemp) 70 80) 50
  , Run $ Memory (cmdParams (tmpl TMemory) 20 90) 50
  , Run $ Battery (cmdParams' (tmpl TBattery) 20 90 ++ batteryExtraParams) 50
  , Run $ Date (fc "%F %T" Blue) "date" 10
  , Run $ Com "bash" ["-c", "echo $(dnf check-update -y | grep -Ec \"updates\")"] "dnfupdates" 18000
  ]

config :: Config
config =
  defaultConfig
    { font = font'
    , alpha = bgAlpha
    , bgColor = color Bg
    , fgColor = color Fg
    , position = TopSize C 100 32
    , sepChar = "%"
    , alignSep = alignSep'
    , template = template'
    , lowerOnStart = True
    , hideOnStart = False
    , allDesktops = True
    , pickBroadest = False
    , persistent = True
    , commands = commands'
    }

main :: IO ()
main = xmobar config
