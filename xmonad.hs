import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.SetWMName
import qualified XMonad.Hooks.DynamicLog as DLog
import qualified XMonad.Hooks.DynamicBars as Bars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens


import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D


myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"
    Bars.dynStatusBarStartup barCreator barDestroyer

-- colours
normBord :: String
normBord = "#4c566a"

focdBord :: String
focdBord = "#5e81ac"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myTerminal :: String
myTerminal = "alacritty"

myEditor :: String
myEditor = "emacsclient -c -a emacs"

myBrowser :: String
myBrowser = "google-chrome-stable"

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]

myBaseConfig = desktopConfig

-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
    -- doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]




myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ lessBorders OnlyScreenFloat $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| ThreeColMid 1 (3/100) (1/2)
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]


-- keys config
myKeys =
  -- Spawn the essentials
  [ ("M-b", spawn $ myBrowser)
  , ("M-e", spawn $ myEditor)
  , ("M-h", spawn $ myTerminal ++ " -e htop" )
  , ("M-t", spawn $ myTerminal )
  , ("M-v", spawn $ "pavucontrol" )
  , ("M-u", spawn $ "arcolinux-logout" )

  -- Rofi
  , ("M-r", spawn $ "rofi -show run" )
  , ("M-s", spawn $ "rofi -show ssh" )
  , ("M-w", spawn $ "rofi -show window" )

  -- Xmonad
  , ("M-S-r", spawn $ "xmonad --recompile && xmonad --restart")

  -- Layouts
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-f", sendMessage $ Toggle NBFULL)
  -- , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Windows
  , ("M-x", kill)
  , ("M-q", kill)
  , ("M-c", nextScreen)
  , ("M-y", prevScreen)

  -- Window focus for both qwerty and rsthd
  , ("M-j", windows W.focusDown)
  , ("M-n", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-a", windows W.focusUp)
  , ("M-m", windows W.focusMaster)

  -- Window swapping for both qwerty and rsthd
  , ("M-S-j", windows W.swapDown)
  , ("M-S-n", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-a", windows W.swapUp)
  , ("M-S-m", windows W.swapMaster)

  -- Change window size
  , ("M-l", sendMessage Shrink)
  , ("M-i", sendMessage Expand)

  -- Copy and paste
  -- , ((0, xF86XK_Copy), copyString)
  -- , ((0, xF86XK_Paste), pasteString)

  -- Media keys
  , ("<XF86AudioMute>", spawn $ "amixer -q set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn $ "amixer -q set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn $ "amixer -q set Master 5%+")
  , ("<XF86MonBrightnessUp>",  spawn $ "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn $ "xbacklight -dec 5")
  , ("<XF86AudioPlay>", spawn $ "playerctl play-pause")
  , ("<XF86AudioNext>", spawn $ "playerctl next")
  , ("<XF86AudioPrev>", spawn $ "playerctl previous")
  , ("<XF86AudioStop>", spawn $ "playerctl stop")

  ---------------------------------------------------------
  -- LEADER KEY SEQUENCES
  -- Window actions => <leader> w -
  , ("M-g w s", withFocused $ windows . W.sink)
  , ("M-g w i", sendMessage (IncMasterN 1))
  , ("M-g w d", sendMessage (IncMasterN (-1)))
  -- Setting apps => <leader> s -
  , ("M-g s t", spawn $ "arcolinux-tweak-tool")
  , ("M-g s m", spawn $ "xfce4-settings-manager")
  -- Open generic apps => <leader> o -
  , ("M-g o d", spawn $ "discord --no-sandbox")
  , ("M-g o t", spawn $ "thunar")
  , ("M-g o f", spawn $ "firefox")
  , ("M-g o g", spawn $ "google-chrome-stable")
  , ("M-g o n", spawn $ "nitrogen")
  , ("M-g o m", spawn $ "pamac-manager")
  , ("M-g o s", spawn $ "spotify")
  -- Toggles => <leader> t -
  , ("M-g t m", spawn $ "amixer set Capture toggle")
  , ("M-g t p", spawn $ "$HOME/.xmonad/scripts/picom-toggle.sh")

  --SCREENSHOTS
  , ("<Print>", spawn $ "flameshot gui")
  , ("C-<Print>", spawn $ "escrotum -C -s" )
  , ("C-S-<Print>", spawn $ "escrotum -C -s" )
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-ctrl-[1..9], Move client to workspace N
  [("M-" ++ m ++ k, windows $ f i)
   | (i, k) <- zip myWorkspaces ["1","2","3","4","5","6","7","8","9","0"]
      , (f, m) <- [(W.greedyView, ""), (W.shift, "C-")]]

-- Xmobar config
myLogPP :: DLog.PP
myLogPP = DLog.defaultPP
    { DLog.ppCurrent = DLog.xmobarColor "#98be65" "" . DLog.wrap "[" "]" -- Current workspace in xmobar
    , DLog.ppVisible = DLog.xmobarColor "#98be65" ""                -- Visible but not current workspace
    , DLog.ppHidden = DLog.xmobarColor "#82AAFF" "" . DLog.wrap "*" ""   -- Hidden workspaces in xmobar
    , DLog.ppHiddenNoWindows = DLog.xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
    , DLog.ppTitle = DLog.xmobarColor "#b3afc2" "" . DLog.shorten 60     -- Title of active window in xmobar
    , DLog.ppSep =  "<fc=#666666> | </fc>"          -- Separators in xmobar
    , DLog.ppUrgent = DLog.xmobarColor "#C45500" "" . DLog.wrap ">" "<"  -- Urgent workspace
    }

barCreator :: Bars.DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid

barDestroyer :: Bars.DynamicStatusBarCleanup
barDestroyer = return ()

main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad . ewmh $
      myBaseConfig
        { startupHook = myStartupHook
        , layoutHook = gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout ||| layoutHook myBaseConfig
        , logHook = Bars.multiPP myLogPP myLogPP
        , manageHook = ( isFullscreen --> doFullFloat ) <+> manageSpawn <+> myManageHook <+> manageHook myBaseConfig
        , modMask = myModMask
        , borderWidth = myBorderWidth
        , terminal = myTerminal
        , handleEventHook    = handleEventHook myBaseConfig <+> fullscreenEventHook <+> Bars.dynStatusBarEventHook barCreator barDestroyer
        , focusFollowsMouse = myFocusFollowsMouse
        , workspaces = myWorkspaces
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        , mouseBindings = myMouseBindings
        } `additionalKeysP` myKeys
