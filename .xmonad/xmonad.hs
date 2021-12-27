-- xmonad.hs

import           XMonad

import Control.Monad (when)
import Data.Maybe
import Data.List (intercalate)
import System.IO
import System.Posix.Unistd
import qualified Data.Map as M

import XMonad.Actions.CycleWS
import XMonad.Actions.Submap (submap)
import XMonad.Prompt.Input
import XMonad.Prompt (XPConfig(..))
import XMonad.Actions.Search
import XMonad.Config.Gnome
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders    (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat
import XMonad.StackSet            (focusDown, view)
import qualified XMonad.Util.ExtensibleState           as XS
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
      myIgnores = ["desktop","desktop_window","stalone-tray","notify-osd","stalonetray","trayer"]
      myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

      -- a trick for fullscreen but stil allow focusing of other WSs
      myDoFullFloat :: ManageHook
      myDoFullFloat = doF focusDown <+> doFullFloat
      -- Lockdown mode (for Getting Work Done)

data LockdownState = LockdownState Bool
  deriving (Typeable, Read, Show)

instance ExtensionClass LockdownState where
  initialValue  = LockdownState False
  extensionType = PersistentExtension

setLockdown :: X ()
setLockdown = XS.put (LockdownState True)

releaseLockdown :: X ()
releaseLockdown = XS.put (LockdownState False)

toggleLockdown :: X ()
toggleLockdown = XS.modify (\(LockdownState l) -> LockdownState (not l))

withLockdown act = do
  LockdownState l <- XS.get
  when (not l) act

viewWeb :: X ()
viewWeb = withLockdown $ windows (view "web")

newManageHook = myManageHook <+> manageHook def

myStartupHook :: X ()
myStartupHook = do
  h <- liftIO getHost
  ewmhDesktopsStartup >> setWMName "LG3D"  --- make java applications work..
  spawnOnce "stalonetray --dockapp-mode simple"
  spawnOnce "setxkbmap -option caps:swapescape"
  spawnOnce "feh --bg-scale ~/Desktop/background.jpg"
  spawnOnce "unity-settings-daemon"
  spawnOnce "gnome-settings-daemon"
  spawnOnce "nm-applet"
  spawnOnce "pasystray"
  spawnOnce myTerminal
  -- spawnOnce "firefox"
  spawnOnce "brave-browser"
  -- spawnOnce "google-chrome --profile-directory=Default"
  spawnOnce "emacs"
  -- spawnOnce "slack"
  -- spawnOnce "discord"
  -- spawnOnce "thunderbird"
  -- spawnOnce "discord"
  -- spawnOnce "thunderbird"
  -- spawnOnce "spotify"
  spawnOnce "xscreensaver -nosplash"
  spawnOnce "screenstopper" -- ~/bin/screenstopper
  if h == Desktop then desktopHooks else laptopHooks
  where
    desktopHooks = do
      spawnOnce "monitors"
      spawnOnce "nordvpn connect"

    laptopHooks = do
      spawnOnce "fdpowermon"
      spawnOnce "blueman-applet"

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
    _               -> Desktop

main = do
  h <- getHost
  xmproc <- spawnPipe $ "/usr/bin/xmobar " <> getXmobarLocation h
  xmonad $ gnomeConfig
    { borderWidth        = 2
    , manageHook         = newManageHook
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , layoutHook         = smartBorders $ myLayout
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , handleEventHook    = fullscreenEventHook <+> docksEventHook
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
    , ((modMask, xK_p)        , spawn "dmenu_run")
    -- select screenshot
    , (modCtrl xK_Print       , spawn mySelectScreenShot)
    -- searches
    , (modCtrl xK_g           , spawn myScreenGif)
    , (modShift xK_n          , spawn "nm-connection-editor")
    , (modCtrl  xK_Right      , nextWS)
    , (modShift xK_Right      , shiftToNext)
    , (smash xK_o             , spawn "pavucontrol")
    , (modCtrl  xK_Left       , prevWS)
    , (modShift xK_Left       , shiftToPrev)
    , ((0, 0x1008ff12        ), spawn "amixer -q set Master mute")    --- can use 'xev' to see key events
    , ((0, 0x1008ff11        ), spawn "amixer -q sset Master 2%- unmute")
    , ((0, 0x1008ff13        ), spawn "amixer -q sset Master 2%+ unmute")
    , ((0, 0x1008ff03        ), spawn "xbacklight -inc -10%")
    , ((0, 0x1008ff02        ), spawn "xbacklight -inc +10%")
    ] `additionalKeysP`
    [ ("M-/", submap . mySearchMap $ myPromptSearch)
    , ("M-g h", visitGithub "tippenein/home")
    , ("M-g l", visit lumiIssues Nothing)
    ]

  where
    smash x = (mod1Mask .|. mod4Mask .|. controlMask, x)
    modMask = myModMask
    modShift x = (modMask .|. shiftMask, x)
    modCtrl x = (modMask .|. controlMask, x)

visitGithub r = visit "https://github.com/" (Just r)

-- visit "somewhere.com" (Just "route")
-- >>> somewhere.com/route
visit :: String -> Maybe String -> X ()
visit url route = safeSpawn mainBrowser [intercalate "/" $ catMaybes [Just url, route]] >> viewWeb

lumiIssues = "https://gitlab.com/lumi-tech/lumi/-/issues?scope=all&state=opened&assignee_username[]=tippenein"

issuesSearch = searchEngineF "lumi-issues" $ \q -> "https://gitlab.com/lumi-tech/lumi/-/issues/" <> q
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


myPromptSearch (SearchEngine _ site) = inputPrompt myXPConfig "Search" ?+ \s ->
  search mainBrowser site s >> viewWeb

mainBrowser = "brave-browser"

getXmobarLocation h = case h of
  Desktop ->
    "~/.xmonad/xmobarDesktop.hs"
  Laptop ->
    "~/.xmonad/xmobarLaptop.hs"

----------------
-- constants ---
----------------
myModMask = mod4Mask -- mod1Maks = alt   |   mod4Mask == meta
myTerminal = "gnome-terminal"
myFocusedBorderColor = "#88bb77"
myNormalBorderColor  = "#003300"
myScreensaver Laptop = "xscreensaver-command -lock"
myScreensaver Desktop = "xscreensaver-command -activate"
mySelectScreenShot = "sleep 0.2; scrot -s -e 'mv $f ~/screenies'"
myFullScreenShot = "scrot -e 'mv $f ~/screenies'"

-- ctrl-shift s to stop recordmydesktop
myScreenGif = intercalate " && "
  [ "mplayer -ao null ./out.ogv -vo jpeg:outdir=/tmp/output"
  , "convert /tmp/output/* /tmp/output.gif"
  , "gifsicle --batch --optimize=3 --scale=0.5 --colors=256 /tmp/output.gif --output ~/screenies/screen-cast-`date +%Y-%m-%d:%H:%M:%S`.gif"
  -- "&& convert /tmp/output.gif -fuzz 10% -layers Optimize ~/screenies/screen-cast-`date +%Y-%m-%d:%H:%M:%S`.gif"
  , "mv ~/out.ogv /tmp/screen-cast-original-`date +%Y-%m-%d:%H:%M:%S`.ogv"
  ]
