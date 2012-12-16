-- ~/.xmonad/xmonad.hs
-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run


import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "xterm"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask
-- Define workspaces
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
-- Dzen config
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1208' -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
--myBtmStatusBar = "conky -c /home/cwills/.xmonad/conky.conf | dzen2 -x '1236' -y '0' -h '24' -w '128' -ta 'r' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myBtmStatusBar = "bash /home/wbrozas/.xmonad/clock.sh | dzen2 -x '1208' -y '0' -h '16' -w '158' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
--myBitmapsDir = "/home/serrghi/.xmonad/dzen"
myBitmapsDir = "/home/wbrozas/.xmonad/dzen"
--}}}
-- Main {{{
main = do
    dzenTopBar <- spawnPipe myStatusBar
    dzenBtmBar <- spawnPipe myBtmStatusBar
    spawn "bash /home/wbrozas/.xmonad/xmonad-startup.sh"
    xmonad $ defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      --, modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "LG3D"
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenTopBar >> fadeInactiveLogHook 0xdddddddd  >> setWMName "LG3D"
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
}
--}}}


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "3:web"    |   c   <- myWebs   ] -- move webs to webs
    , [className    =? c            --> doShift  "4:dev"    |   c   <- myDevs   ] -- move devs to devs
    , [className    =? c            --> doF(W.shift "6:wine")   |   c   <- myWines  ] -- move wines to wine
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail"]
        myDevs    = ["Eclipse","eclipse","Netbeans","Gvim"]
        myWines   = ["Wine"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
layoutHook' = customLayout

-- Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#161616" . pad
      , ppVisible           =   dzenColor "white" "#161616" . pad
      , ppHidden            =   dzenColor "white" "#161616" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#161616" . pad
      , ppUrgent            =   dzenColor "red" "#161616" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#161616" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "SimplestFloat"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#161616" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- Layout
-- customLayout = gaps [(D,16)] $ avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full ||| smartBorders simpleFloat
customLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full ||| smartBorders simplestFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1   
    delta   = 2/100
    ratio   = 1/2
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
 
colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#ebac54"
-- colorFocusedBorder   = "#000000"
barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 20
                }
-- }}}
-- Key mapping {{{
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    
    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    
    , ((modm,               xK_p     ), spawn "xterm")

    -- launch gmrun
    , ((modm, xK_r     ), spawn "gmrun")

    -- launch browser
    , ((modm, xK_w     ), spawn "/home/wbrozas/firefox/firefox")

    -- launch screensaver
    , ((modm, xK_F12     ), spawn "xlock")

    -- launch alsamixer
    , ((modm, xK_e     ), spawn "xterm -e alsamixer")

    -- launch alsamixer
    , ((modm .|. shiftMask, xK_e     ), spawn "xterm -e wicd-curses")
    
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    --, ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    
    ++

    --
    -- mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,s,d}, Move client to screen 1, 2, or 3
    --
    
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
