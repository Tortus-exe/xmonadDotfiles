{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Xmobar
import Data.Char
import Data.List
import Data.Maybe
import XMonad.Util.Run
import Data.Bifunctor
import System.IO.Unsafe

format :: (String, String, String, String,String) -> String
format (action,fg,bg,text,aft) = case (action,fg,bg) of 
        ("","","")->    text
        ("",fg,"")->    foldl1(++) [                              "<fc=",fg,       ">",text,aft,"</fc>"]
        ("",fg,bg)->    foldl1(++) [                              "<fc=",fg,",",bg,">",text,aft,"</fc>"]
        (action,"","")->foldl1(++) [                  "<action=1",action,"` button=1>",text,"</action>",aft]
        (action,fg,bg)->foldl1(++) ["<fc=",fg,",",bg,"><action=`",action,"` button=1>",text,"</action>",aft,"</fc>"]
        

getFromXres :: String -> IO String
getFromXres key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$> (
                find ((== xresKey) . fst)
                $ catMaybes
                $ splitAtColon
                <$> lines xres
              )

    splitAtColon :: String -> Maybe (String, String)
    splitAtColon str = splitAtTrimming str <$> (elemIndex ':' str)

    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming str idx = bimap trim trim . (second tail) $ splitAt idx str

    trim :: String -> String
    trim = dropWhileEnd (isSpace) . dropWhile (isSpace)
-- perform the IO action and return the final string based on a key
fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

xmobarBG = (fromXres "*.background")
xmobarFG = (fromXres "*.foreground")

volumeColor =   "#2b005e"
calColor =      "#41230e"
wifiNameColor = "#400c0c"
memColor =      "#15403b"
batColor =      "#17456b"
brightColor =   "#7d5200"
cpuColor =      "#7d2d00"
fg =            xmobarFG --"#d2c2be"

--  \57958
--  \63578
--  \61931
--  \63540
--  \63055
--  \57526
--  \62722
--  \57524

config :: Config
config = defaultConfig { font = "FiraCode Nerd Font Mono 17"
       , bgColor = xmobarBG
       , fgColor = xmobarFG
       , alpha = 255 -- if this is turned off, then background colours will not work
       , position = TopSize L 130 29
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
          Run $ Network "wlan0" ["--template", "↑<txvbar>↓<rxvbar>", "-L","1000","-H","5000", "--low", "gray", "--normal","green","--high","red"] 10
        , Run $ Cpu ["-t","\63578<total>","-L","3","-H","50","--high","red"] 10
        , Run $ Memory ["-t","\57958<usedratio>"] 10
        , Run $ Swap [] 10
        , Run $ Date "%a %Y-%m-%d %H:%M:%S" "date" 10
        --, Run $ Wireless "wlan0" ["-t", "\61931 <qualityvbar> <essid>"] 10
        , Run $ Com "currentWifi" [] "wifi" 30
        , Run $ Com "nextclass" [] "nextclass" 900
        , Run $ XMonadLog
        , Run $ Brightness ["-t", "\63540<percent>", "--", "-D", "amdgpu_bl0"] 60
        , Run $ BatteryP ["BATT"] ["-t", "<watts> \63055<left><leftvbar>", 
                                "-a", "notify-send -u critical \"Battery Low!\"", 
                                "-L", "10", 
                                "-H", "20", 
                                "--low", "red", 
                                "--normal", "yellow", 
                                "high", "green"] 10
        ]
       , lowerOnStart = False
       , sepChar = "%"
       , alignSep = "}{"
       , template = foldl1 (++) $ map format [
       ("","","","%XMonadLog% %nextclass%}{",""), 
       ("",volumeColor,"","\57526",""),
       ("kitty -e pulsemixer",fg,volumeColor,"墳 ",""),
       ("qjackctl",fg,volumeColor,"\62722 ",""),
       ("",brightColor,volumeColor,"\57526",""),
       ("",fg,brightColor,"%bright%",""),
       ("",batColor,brightColor,"\57526",""),
       ("kitty -e batmenu",fg,batColor,"%battery%",""),
       ("",cpuColor,batColor,"\57526",""),
       ("",fg,cpuColor,"%cpu%",""),
       ("",memColor,cpuColor,"\57526",""),
       ("",fg,memColor,"%memory%",""),
       ("",wifiNameColor,memColor,"\57526",""),
       ("kitty -e iwctl",fg,wifiNameColor,"%wifi%",""),
       ("",xmobarBG,wifiNameColor,"\57526",""),
       ("","","","%wlan0%",""),
       ("",calColor,"","\57526",""),
       ("kitty -e calcurse",fg,calColor,"%date%","")
        ]}

main :: IO ()
main = xmobar config
