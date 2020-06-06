-- System
import System.IO

-- GHC
import GHC.Word (Word32)

-- Data
import Data.Monoid

-- XMonad
import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Settings
myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Word32
myBorderWidth = 1

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#222222"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ffffff"

-- Startup Hook
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"

-- Layout
myLayout = avoidStruts (smartBorders $ fullscreenFull $ tiled ||| Mirror tiled ||| noBorders Full)
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
   isFullscreen --> doFullFloat,
   manageHook defaultConfig
 ]

-- Keys
myKeys :: [([Char], X ())]
myKeys = 
  -- Xmonad hotkeys
  [ ("M-d", kill)
  
    -- Open my preferred terminal. 
  , ("M-<Return>", spawn (myTerminal ++ " -e fish"))

    -- My applications
  , ("M-f", spawn "firefox")
  , ("M-e", spawn "emacs")
  , ("M-s", spawn "~/.dmenu/dmenu-scrot.sh")
  , ("M-i", spawn "~/.dmenu/dmenu-cmus.sh")

    -- Kill xmobar 
  , ("M-u", spawn "killall xmobar")

    -- Multimedia keys
  , ("M-,", spawn "cmus-remote -r")
  , ("M-.", spawn "cmus-remote -n")
  , ("M-/", spawn "~/.config/scripts/cmus-play-pause.sh")
  ]


-- Main
main :: IO ()
main = do 
    xmproc <- spawnPipe "xmobar -x 0 /home/notation/.config/xmobar/xmobar.config"
    xmonad $ fullscreenSupport $ docks defaultConfig
        { layoutHook =          myLayout
        , logHook =             dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc
                                , ppTitle = xmobarColor "white" "" . shorten 50
                                , ppCurrent = xmobarColor "#FFA500" ""
                                , ppSep =  "<fc=#666666> | </fc>"
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
