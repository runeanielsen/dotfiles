import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import System.IO

-- Settings
myModMask = mod4Mask
myBorderWidth = 1 
myTerminal = "alacritty"
myNormalBorderColor = "#222222"
myFocusedBorderColor = "#ffffff"

-- Startup Hook
myStartupHook = do
	spawnOnce "nitrogen --restore &"

-- Layout
myLayout = avoidStruts (smartBorders $ tiled ||| Mirror tiled ||| noBorders Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Main
main = do   
    xmproc <- spawnPipe "xmobar -x 0 /home/notation/.config/xmobar/xmobar.config"
    xmonad $ docks defaultConfig
        { layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = myModMask
	, borderWidth = myBorderWidth 
 	, startupHook = myStartupHook
        , terminal = myTerminal
	, normalBorderColor = myNormalBorderColor
	, focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
