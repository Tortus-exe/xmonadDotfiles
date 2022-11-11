{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Actions.GridSelect

import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.Renamed

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Spacing

import System.Exit

import qualified XMonad.StackSet as W

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.Maybe
import Data.Bifunctor
import Data.List as DL
import Data.Char as DC
import Data.Map.Strict
import Data.Monoid
import Control.Monad

import System.IO
import System.IO.Unsafe



c_systemTerminal = "kitty"
c_focusFollowsMouse = True
c_clickJustFocuses = False
c_windowBorder = 0 --px
c_windowBorderColor = fromXres "*.color0"
c_focusBorderColor = fromXres "*.color8"
c_wallpaper = "~/Pictures/Moebius4.jpeg"

c_superKey = mod4Mask
--               name,keycombo to goto
c_Workspaces = [ ("1","M-1"),
                 ("2","M-2"),
                 ("3","M-3"),
                 ("4","M-4"),
                 ("5","M-5"),
                 ("6","M-6"),
                 ("7","M-7"),
                 ("8","M-8"),
                 ("9","M-9")]
-- screen no.            0      1      2
c_PhysicalScreenCmds = ["M-w", "M-e", "M-r"]


-- used to transform the workspace goto keybinding into the workspace moveto keybinding
insertShift :: String -> String
insertShift x = DL.take 2 x ++ "S-" ++ DL.drop 2 x

windowTilingLayout = avoidStruts $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $ (blocked ||| renamed [Replace "\63378"] (Full))
  where
          blocked = renamed [Replace "\63424"] $ ResizableThreeCol 1 (3/100) (1/2) []

newWindowHook :: ManageHook
newWindowHook = composeAll
--        [ className =? "MPlayer" --> doFloat
--        , className =? "Gimp"    --> doFloat]
        [ isDialog --> doFloat ]


-- xEventHook :: Event -> X All
-- xEventHook = mempty


keybindings :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
keybindings = \c -> mkKeymap c $ 
        [ ("M-S-<Return>", spawn $ XMonad.terminal c)
        , ("M-<F5>", spawn "xbacklight +5")
        , ("M-<F4>", spawn "xbacklight -5")
        , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
        , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
        , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
        , ("<Print>", unGrab *> spawn "scrot -s")
        , ("M-p", spawn "dmenu_run -c -l 14 -g 4")
        , ("M-S-c", kill)
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
        , ("M-n", refresh) --resize windows to correct size
        , ("M-<Tab>", windows W.focusDown)
        , ("M-u", windows W.focusDown)
        , ("M-o", windows W.focusUp)
        , ("M-m", windows W.focusMaster)
        , ("M-<Return>", windows W.swapMaster)
        , ("M-l", windows W.swapDown)
        , ("M-j", windows W.swapUp)
        , ("M-<L>", sendMessage $ IncMasterRows 1)
        , ("M-<R>", sendMessage $ IncMasterRows (-1))
        , ("M-<U>", sendMessage $ IncMasterCols 1)
        , ("M-<D>", sendMessage $ IncMasterCols (-1))
        , ("M-S-j", sendMessage Shrink)
        , ("M-S-i", sendMessage MirrorExpand)
        , ("M-S-l", sendMessage Expand)
        , ("M-S-k", sendMessage MirrorShrink)
        , ("M-t", withFocused $ windows . W.sink)
        , ("M-,", sendMessage (IncMasterN 1))
        , ("M-.", sendMessage (IncMasterN (-1)))
        , ("M-S-q", io (exitWith ExitSuccess))
        , ("M-q", spawn "xmonad --recompile && xmonad --restart")
        , ("M-b", sendMessage ToggleStruts)
        , ("M-S-/", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
        , ("M-S-.", goToSelected customGridSelectConfig)
        ]
        ++
        -- switching to workspace numbered in c_Workspaces based on command given there
        [(cmd, windows $ W.greedyView ws)
           | (ws, cmd) <- c_Workspaces]
        ++
        -- moving something to that workspace instead of viewing it
        [(insertShift cmd, windows $ W.shift ws)
           | (ws, cmd) <- c_Workspaces]
        ++
        -- switching to physical screen 0, 1, 2
        [(cmd, screenWorkspace wspcNo >>= flip whenJust (windows . W.view))
           |(cmd, wspcNo) <- zip c_PhysicalScreenCmds [0..]]
        ++ 
        -- moving thing to physical screen 0, 1, 2
        [ (insertShift cmd, screenWorkspace wspcNo >>= flip whenJust (windows . W.shift))
           | (cmd, wspcNo) <- zip c_PhysicalScreenCmds [0..]]

-- mouse :: !(XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
windowSetChangeHook :: X ()
windowSetChangeHook = fadeInactiveLogHook 1

-- function to get something from .Xresources file
getFromXres :: String -> IO String
getFromXres key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$> (
                DL.find ((== xresKey) . fst)
                $ catMaybes
                $ splitAtColon
                <$> lines xres
              )

    splitAtColon :: String -> Maybe (String, String)
    splitAtColon str = splitAtTrimming str <$> (DL.elemIndex ':' str)

    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming str idx = bimap trim trim . (second tail) $ DL.splitAt idx str

    trim :: String -> String
    trim = DL.dropWhileEnd (DC.isSpace) . DL.dropWhile (DC.isSpace)
-- perform the IO action and return the final string based on a key
fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres


xmobarBG = (fromXres "*.background")
xmobarFG = (fromXres "*.foreground")

myStartupHook :: X ()
myStartupHook = do
        --spawnOnce "nitrogen --restore &"
        spawnOnce ("feh --bg-fill "++ c_wallpaper++" &")
        spawnOnce "picom --shadow-opacity 0 --corner-radius 5 &"
        spawnOnce "xrandr --output eDP-1 --mode 1920x1200 --preferred &"

--https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-Core.html#t:XConfig
configuration = def {
            normalBorderColor = c_windowBorderColor
          , focusedBorderColor = c_focusBorderColor
          , terminal = c_systemTerminal
          , layoutHook = windowTilingLayout
            -- manageHook = newWindowHook
            -- handleEventHook = xEventHook
          , workspaces = DL.map fst c_Workspaces
          , modMask = c_superKey
          , XMonad.keys = keybindings
            -- mouseBindings = mouse
          , borderWidth = c_windowBorder
          , logHook = windowSetChangeHook
          , startupHook = myStartupHook
          , focusFollowsMouse = c_focusFollowsMouse
          , clickJustFocuses = c_clickJustFocuses
            -- clientMask
            -- rootMask
            -- handleExtraArgs
            -- extensibleConf
                    } 

customXMobarPP :: PP
customXMobarPP = def {
            ppTitleSanitize = xmobarStrip 
          , ppSep = "  "
          , ppCurrent = \_ -> activeWindowColor "\60017"
                  --("<fc="++(fromXres "*.color3") ++ ">\57526</fc><fc=" ++ (xmobarBG)++","++(fromXres "*.color3")++">")++("</fc><fc=" ++ (fromXres "*.color3") ++ ">\xe0b4</fc>")
          , ppHidden = hiddenWindowColor . \_ -> "\60092"
          , ppHiddenNoWindows = invisible . wrap "" ""
          , ppVisible = activeWindowColor . \_ -> "\60298"
          , ppOrder = \(ws:l:_:_) -> [ws,l]
          , ppLayout = DL.drop 8 -- remove the word "spacing "
                     }
        where
          normalColor, activeWindowColor, currentWindowColor :: String -> String
          normalColor = xmobarColor (fromXres "*.color7") ""
          activeWindowColor = xmobarColor (fromXres "*.color3") ""
          currentWindowColor = xmobarColor xmobarBG (fromXres "*.color3")
          hiddenWindowColor = xmobarColor (fromXres "*.color8") ""
          invisible = xmobarColor xmobarBG xmobarBG

customGridSelectConfig :: GSConfig Window
customGridSelectConfig = (buildDefaultGSConfig colorizer) { 
               gs_cellheight = 100
             , gs_cellwidth = 300
             , gs_font = "xft:Fira Code Regular Nerd Font Complete"
                             }

colorizer :: Window -> Bool -> X(String, String)
colorizer = colorRangeFromClassName
                     black            -- lowest inactive bg
                     (0xFF,0x70,0xFF) -- highest inactive bg
                     (88, 3, 138)            -- active bg
                     (77,77,77) -- inactive fg
                     white            -- active fg
  where black = minBound
        white = maxBound

main :: IO ()
main = xmonad
     . withSB (statusBarProp ("xmobar")
                        (pure customXMobarPP))
     . ewmhFullscreen
     . ewmh
     . docks
     $ configuration


help :: String
help = unlines ["Windows is the mod key.",
                "",
                "⊞⇧⏎  terminal",
                "⊞ p       dmenu",
                "⊞⇧ c      close"
               ]
