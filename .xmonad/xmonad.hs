import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import System.IO

-- Settings
myModMask =             mod4Mask
myBorderWidth =         1 
myTerminal =            "alacritty"
myNormalBorderColor =   "#222222"
myFocusedBorderColor =  "#ffffff"

-- Startup Hook
myStartupHook = return ()

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
myManageHook = composeAll [
   manageDocks,
   isFullscreen --> doFullFloat,
   manageHook defaultConfig
 ] 

-- Main
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
        }
