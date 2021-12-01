import XMonad
import XMonad.Prompt (XPrompt(..), mkComplFunFromList, mkXPrompt, XPConfig(..))
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Spacing (spacingWithEdge)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad . ewmh $ desktopConfig
      { manageHook = manageDocks <+> manageHook desktopConfig <+> (isFullscreen --> doFullFloat)
      , layoutHook = spacingWithEdge 10 $ avoidStruts $ layoutHook desktopConfig
      , logHook = dynamicLogWithPP xmobarPP
                      { ppOutput = hPutStrLn xmproc
                      , ppTitle = xmobarColor "green" "" . shorten 50
                      }
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
      , handleEventHook = handleEventHook desktopConfig <+> fullscreenEventHook
      , borderWidth = 0
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
      , ((mod4Mask .|. shiftMask, xK_f),
          mkXPrompt
            FirefoxSearch
            xpromptConfig
            (mkComplFunFromList commonWebsites)
            (\domain -> spawn ("firefox \"https://" <> domain <> "\""))
        )
      , ((mod4Mask .|. shiftMask, xK_s),
          mkXPrompt
            SteamGame
            xpromptConfig
            (mkComplFunFromList (map fst mySteamGames))
          (\x -> case lookup x mySteamGames of
            Just steamId -> spawn ("steam steam://rungameid/" <> steamId)
            Nothing -> spawn "steam"
          )
        )
      , ((mod4Mask .|. shiftMask, xK_h), spawn "firefox ~/Documents/xmonad-keybindings-cheatsheet.png")
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")
      ]

data FirefoxSearch = FirefoxSearch

instance XPrompt FirefoxSearch where
  showXPrompt FirefoxSearch = "https://"

data SteamGame = SteamGame
instance XPrompt SteamGame where
  showXPrompt SteamGame = "Game: "

xpromptConfig :: XPConfig
xpromptConfig = def
  { font = "xft:FontAwesome:pixelsize=20"
  , height = 30
  , showCompletionOnTab = False
  , bgColor = "darkblue"
  , fgColor = "lightblue"
  , bgHLight = "white"
  , fgHLight = "darkblue"
  }

commonWebsites =
  [ "google.com"
  , "hackage.haskell.org"
  , "hoogle.haskell.org"
  , "crates.io"
  , "reddit.com"
  , "mail.google.com"
  , "docs.rs"
  , "github.com"
  , "github.com/SamuelSchlesinger"
  ]

mySteamGames =
  [ ("rimworld", "294100")
  ]
