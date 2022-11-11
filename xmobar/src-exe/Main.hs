{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Xmobar
import Data.Char
import Data.List
import Data.Maybe
import XMonad.Util.Run
import Data.Bifunctor
import System.IO.Unsafe

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

--  \57958
--  \63578
--  \61931
--  \63540
--  \63055
--  \57526
--  \62722
--  \57524

config :: Config
config = defaultConfig { font = "FiraCode Nerd Font Mono 16"
       , bgColor = xmobarBG
       , fgColor = xmobarFG
       , alpha = 255 -- if this is turned off, then background colours will not work
       , position = TopSize L 130 25
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
         Run $ Network "wlan0" ["--template", "↑<tx>↓<rx>", "-L","1000","-H","5000", "--low", "gray", "--normal","green","--high","red"] 10
        , Run $ Cpu ["-t","\57958<total>","-L","3","-H","50", "--normal","green","--high","red"] 10
        , Run $ Memory ["-t","\63578<usedratio>"] 10
        , Run $ Swap [] 10
        , Run $ Date "%a %Y-%m-%d %H:%M:%S" "date" 10
        --, Run $ Wireless "wlan0" ["-t", "\61931 <qualityvbar> <essid>"] 10
        , Run $ Com "currentWifi" [] "wifi" 30
        , Run $ Com "nextclass" [] "nextclass" 900
        , Run $ XMonadLog
        , Run $ Brightness ["-t", "\63540<percent>", "--", "-D", "amdgpu_bl0"] 60
        , Run $ BatteryP ["BATT"] ["-t", "<watts> \63055<left>(<timeleft>)", 
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
       , template = "%XMonadLog% %nextclass%}{<fc=#2b005e>\57526</fc><fc=#D2C2be,#2b005e><action=`kitty -e pulsemixer`>墳</action> <action=`qjackctl` button=1>\62722</action></fc><fc=#2b005e>\57524</fc> %bright% <action=`kitty -e batmenu`><fc=#D2C2BE,#400c0c>%battery%</fc></action> %cpu% <fc=#d2c2be,#15403b>%memory%</fc> <action=`kitty -e iwctl`>%wifi%</action> %wlan0% <action=`kitty -e calcurse`><fc=#d2c2be,#41230e>%date%</fc></action>" }

main :: IO ()
main = xmobar config
