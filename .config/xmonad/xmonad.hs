-- xmonad.hs

import XMonad

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Posix.Unistd

import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.Submap (submap)
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt (XPConfig(..))
import XMonad.Prompt.Input
import XMonad.StackSet (focusDown, view)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
-------------------
-- Layouts --------
-------------------
myLayout = avoidStruts $ layoutHook def

-------------------
-- Workspace names -
-------------------
myWorkspaces = [ "1:shell"
               , "2:emacs"
               , "3:web"
               , "4:biz"
               , "5:chat"
               , "6:other"
               , "7"
               , "8"
               , "9:logs"]
-------------------
-- Hooks ----------
-------------------
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ pure $ manageHook gnomeConfig
    , [ resource     =? r --> doIgnore            | r <- myIgnores]
    , [ className    =? c --> doShift  "1:shell"  | c <- myShell  ]
    , [ className    =? c --> doShift  "2:emacs"  | c <- myDev    ]
    , [ className    =? c --> doShift  "3:web"    | c <- myWeb    ]
    , [ className    =? c --> doShift  "4:biz"    | c <- myBiz    ]
    , [ className    =? c --> doShift  "5:chat"   | c <- myChat   ]
    , [ className    =? c --> doShift  "6:other"  | c <- myOther  ]
    , [ className    =? c --> doShift  "7"        | c <- myGames  ]
    , [ className    =? c --> doCenterFloat       | c <- myFloats ]
    , [ name         =? n --> doCenterFloat       | n <- myNames  ]
    , [ isFullscreen      --> myDoFullFloat                           ]
    , pure manageDocks
    ])

    where
      role      = stringProperty "WM_WINDOW_ROLE"
      name      = stringProperty "WM_NAME"

      -- classnames - Use 'xprop' to click windows and find out classname
      myShell   = ["gnome-terminal", "urxvt", "rxvt-unicode"]
      myDev     = ["emacs", "Emacs"]
      myWeb     = ["Firefox", "firefox-trunk", "brave-browser", "Brave-browser"]
      myBiz     = ["Chromium-browser","chromium-browser", "google-chrome", "Google-chrome"]
      myChat    = ["Pidgin","Buddy List", "hipchat", "HipChat", "Slack"]
      myOther   = ["Evince","xchm","libreoffice-writer","libreoffice-startcenter", "Signal", "Thunderbird", "Spotify"]
      myGames   = ["Slay the Spire"]
      myFloats  = [ "Discord"
                  , "Slack Call Minipanel"
                  , "Zeal"
                  , "keepass2"
                  , "keepassx"
                  , "feh"
                  , "PatchWindow"
                  , "PdWindow"
                  , "Gimp"
                  , "Xmessage"
                  , "XFontSel"
                  , "Nm-connection-editor"
                  , "qbittorrent"
                  , "Steam"
                  , "pavucontrol"
                  ]

      -- resources
      myIgnores = ["desktop","desktop_window","notify-osd","trayer"]
      myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

      -- a trick for fullscreen but stil allow focusing of other WSs
      myDoFullFloat :: ManageHook
      myDoFullFloat = doF focusDown <+> doFullFloat

viewWeb :: X ()
viewWeb = windows (view "web")



myStartupHook :: X ()
myStartupHook = do
  h <- liftIO getHost
  -- spawnOnce myTrayer
  spawnOnce "setxkbmap -option caps:swapescape"
  spawnOnce "feh --bg-scale ~/Desktop/background.jpg"
  spawnOnce "unity-settings-daemon"
  spawnOnce "gnome-settings-daemon"
  spawnOnce "nm-applet"
  spawnOnce "pasystray"
  spawnOnce myTerminal
  spawnOnce "brave-browser"
  spawnOnce "redshift-gtk"
  spawnOnce "emacs"
  spawnOnce "xscreensaver -nosplash"
  if h == Desktop then desktopHooks else laptopHooks
  where
    desktopHooks = do
      spawnOnce "monitors"

    laptopHooks = do
      spawnOnce "fdpowermon"
      -- spawnOnce "blueman-applet"

data Host
  = Desktop
  | Laptop
  deriving (Eq, Read, Show)

-- hostname as identifier
-- https://github.com/byorgey/dotfiles/blob/master/xmonad.hs#L106
getHost :: IO Host
getHost = do
  hostName <- fmap nodeName getSystemID
  return $ case hostName of
    "kodukbunwaree" -> Desktop
    "tippenein"     -> Laptop
    "brady-laptop"  -> Laptop
    _               -> Desktop

main :: IO ()
main = do
  h <- getHost
  xmproc <- spawnPipe $ "/usr/bin/xmobar " <> getXmobarLocation h
  xmonad $ gnomeConfig -- docks (ewmh def)
    { borderWidth        = 2
    , manageHook         = myManageHook <+> manageHook def
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , layoutHook         = smartBorders $ myLayout
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    -- , handleEventHook = docks $ ewmf def
    , focusFollowsMouse  = True
    , logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "green" "" . shorten 40
              }
    }

----------------
-- Keybinds ----
----------------
    `additionalKeys`
    -- screensaver
    [ (modShift xK_z          , spawn $ myScreensaver h)
    -- normal screenshot
    , ((0, xK_Print         ) , spawn myFullScreenShot)
    , ((mm, xK_p)        , spawn "dmenu_run")
    -- select screenshot
    , (modCtrl xK_Print, spawn mySelectScreenShot)
    , (smash xK_Print, spawn mySelectScreenShotDelayed)
    , (modCtrl  xK_Right, nextWS)
    , (modShift xK_Right, shiftToNext)
    , (smash xK_o, safeSpawnProg "pavucontrol")
    , (smash xK_p, safeSpawnProg "gnome-control-center")
    , (modCtrl  xK_Left, prevWS)
    , (modShift xK_Left, shiftToPrev)
    , ((0, 0x1008ff12), spawn "amixer -q set Master mute")    --- can use 'xev' to see key events
    , ((0, 0x1008ff11), spawn "amixer -q sset Master 2%- unmute")
    , ((0, 0x1008ff13), spawn "amixer -q sset Master 2%+ unmute")
    , ((0, 0x1008ff03), spawn "xbacklight -inc -10%")
    , ((0, 0x1008ff02), spawn "xbacklight -inc +10%")
    , (modShift xK_t, teatimer)
    ] `additionalKeysP`
    [ ("M-/", submap . mySearchMap $ myPromptSearch)
    , ("M-g h", visitGithub "tippenein/home")
    , ("M-g l", visit lumiIssues Nothing)
    , ("M-z z", zeal)
    ]

  where
    smash x = (mod1Mask .|. mod4Mask .|. controlMask, x)
    mm = myModMask
    modShift x = (mm .|. shiftMask, x)
    modCtrl x = (mm .|. controlMask, x)
    lumiIssues = "https://gitlab.com/lumi-tech/lumi/-/issues?scope=all&state=opened&assignee_username[]=tippenein"

visitGithub :: String -> X ()
visitGithub r = visit "https://github.com/" (Just r)

-- visit "somewhere.com" (Just "route")
-- >>> somewhere.com/route
visit :: String -> Maybe String -> X ()
visit url route = safeSpawn mainBrowser [intercalate "/" $ catMaybes [Just url, route]] >> viewWeb

issuesSearch :: SearchEngine
issuesSearch = searchEngineF "lumi-issues" $ \q -> "https://gitlab.com/lumi-tech/lumi/-/issues/" <> q

libgenSearch :: SearchEngine
libgenSearch = searchEngineF "libgen" $ \q -> "https://libgen.is/search.php?req=" <> q

-- Search
-- g - google
-- w - wiki
-- h - hoogle
-- i - issues search
mySearchMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
mySearchMap method = M.fromList $
  [ ((0, xK_g), method google)
  , ((0, xK_w), method wikipedia)                         --  "
  , ((0, xK_h), method hoogle)
  , ((0, xK_i), method issuesSearch)
  , ((0, xK_l), method libgenSearch)
  ]

myXPConfig :: XPConfig
myXPConfig = def

zeal :: X ()
zeal = inputPrompt myXPConfig "Zeal" ?+ \s ->
  safeSpawn "zeal" [s]

teatimer :: X ()
teatimer = inputPrompt myXPConfig "Time" ?+ \s ->
  asks (terminal . config) >>= \t -> safeSpawn t ["'/usr/local/bin/sh' -- tt " <> s]

myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site) = inputPrompt myXPConfig "Search" ?+ \s ->
  search mainBrowser site s >> viewWeb

getXmobarLocation :: Host -> String
getXmobarLocation h = case h of
  Desktop ->
    "~/.config/xmonad/xmobarDesktop.hs"
  Laptop ->
    "~/.config/xmonad/xmobarLaptop.hs"

----------------
-- constants ---
----------------
mainBrowser = "brave-browser"
myModMask = mod4Mask -- mod1Maks = alt   |   mod4Mask == meta
myTerminal = "gnome-terminal"
myFocusedBorderColor = "#9c03c8"
myNormalBorderColor  = "#003300"
myScreensaver Laptop = "xscreensaver-command -lock"
myScreensaver Desktop = "xscreensaver-command -activate"
mySelectScreenShot = "sleep 0.2; scrot --select -e 'mv $f ~/screenies'"
mySelectScreenShotDelayed = "sleep 0.2; scrot --select --delay 3 -e 'mv $f ~/screenies'"
myFullScreenShot = "scrot -e 'mv $f ~/screenies'"
-- myTrayer = "trayer --transparent true --edge top --align right --monitor primary --width 12 --iconspacing 2 --tint 0x000000 --height 23"
