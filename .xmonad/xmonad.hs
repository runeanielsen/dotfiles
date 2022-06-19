-- System
import System.IO

-- GHC
import GHC.Word (Word32)

-- Data
import Data.Monoid

-- XMonad
import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Spacing
import XMonad.Layout.CenteredIfSingle (centeredIfSingle)

-- Settings
myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Word32
myBorderWidth = 2

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#4b5467"

myFocusedBorderColor :: String
myFocusedBorderColor = "#8893a9"

-- Startup Hook
myStartupHook :: X ()
myStartupHook = do
  spawn "trayer --edge top --align right --widthtype pixel --height 22px --SetDockType true --SetPartialStrut true --expand true --iconspacing 4 --tint 0x000000 --transparent true --alpha 0 &"
  spawn "xcompmgr -a &"
  spawn "nitrogen --restore &"
  spawn "dunst &"
  spawn "xset r rate 200 25 &"
  spawn "xset s off -dpms &"
  spawn "unclutter -idle 5 &"
  spawn "xdg-settings set default-web-browser firefox.desktop"
  setWMName "LG3D"

-- Layout
myLayout =
  avoidStruts (smartBorders $ spacingWithEdge 5 $ centeredIfSingle 0.60 tiled) |||
  avoidStruts (noBorders $ spacingWithEdge 0 Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Manage Hook
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll [
   manageDocks,
   isFullscreen --> doFullFloat
 ]

-- Keys
myKeys :: [([Char], X ())]
myKeys =
  -- Xmonad hotkeys
  [ ("M-d", kill)

    -- Open my preferred terminal.
  , ("M-<Return>", spawn (myTerminal ++ " -e bash"))

    -- My applications
  , ("M-f", spawn "firefox")
  , ("M-e", spawn "emacs -ib 15")
  , ("M-i", spawn "~/.config/scripts/dmenu-passmenu.sh")
  , ("M-b", spawn "~/.config/scripts/dmenu-books.sh")
  , ("M-v", spawn "~/.config/scripts/dmenu-videos.sh")

    -- Kill xmobar
  , ("M-u", spawn "killall xmobar trayer")

    -- Multimedia keys
  , ("M-,", spawn "cmus-remote -r")
  , ("M-.", spawn "cmus-remote -n")
  , ("M-/", spawn "~/.config/scripts/cmus-play-pause.sh")
  ]

-- Main
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobar.config"
    xmonad $ fullscreenSupport $ docks def
        { layoutHook =          myLayout
        , logHook =             dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc
                                , ppTitle = const ""
                                , ppCurrent = xmobarColor "#FFA500" ""
                                , ppSep =  "<fc=white> | </fc>"
                                , ppHiddenNoWindows = xmobarColor "grey" ""
                                }
        , modMask =             myModMask
        , borderWidth =         myBorderWidth
        , startupHook =         myStartupHook
        , terminal =            myTerminal
        , normalBorderColor =   myNormalBorderColor
        , focusedBorderColor =  myFocusedBorderColor
        , manageHook = myManageHook
        } `additionalKeysP` myKeys
