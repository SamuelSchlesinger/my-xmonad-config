-- XMonad imports
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as StackSet

import XMonad.Prompt (XPrompt(..), mkComplFunFromList, mkXPrompt, XPConfig(..))
import XMonad.Prompt.Man
import XMonad.Prompt.XMonad

import XMonad.Config.Desktop (desktopConfig)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)

import XMonad.Actions.DynamicProjects (switchProjectPrompt, Project(..), dynamicProjects, switchProject)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt)

import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)


-- Base imports
import System.IO

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"

    xmonad . dynamicProjects myProjects $ desktopConfig
      { terminal = myTerminal
      , manageHook =
          manageDocks
          <+> manageHook desktopConfig
          <+> (isFullscreen --> doFullFloat)
      , layoutHook =
          avoidStruts
          $ (   (windowSwitcherDecorationWithImageButtons shrinkText myTheme . draggingVisualizer . spacing 10) (GridRatio (3/2))
            ||| tabbed shrinkText myTheme
            )
      , logHook =
          dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
          }
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
      , handleEventHook =
          handleEventHook desktopConfig
          <+> fullscreenEventHook
      , borderWidth = 0
      , focusedBorderColor = "yellow"
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_f),
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
      , ((mod4Mask .|. shiftMask, xK_h),
          spawn "firefox ~/Documents/xmonad-keybindings-cheatsheet.png"
        )
      , ((mod4Mask .|. shiftMask, xK_l),
          addWorkspacePrompt xpromptConfig
        )
      , ((mod4Mask .|. shiftMask, xK_p),
          switchProjectPrompt xpromptConfig
        )
      , ((mod4Mask .|. shiftMask, xK_m),
          manPrompt xpromptConfig
        )
      , ((mod4Mask .|. shiftMask, xK_x),
          xmonadPrompt xpromptConfig
        )
      , ((controlMask, xK_Print),
          spawn "sleep 0.2; scrot -s"
        )
      , ((0, xK_Print),
          spawn "scrot"
        )
      ]

data FirefoxSearch = FirefoxSearch

instance XPrompt FirefoxSearch where
  showXPrompt FirefoxSearch = "firefox https://"

data SteamGame = SteamGame
instance XPrompt SteamGame where
  showXPrompt SteamGame = "steam steam://rungameid/"

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

commonWebsites :: [String]
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

mySteamGames :: [(String, String)]
mySteamGames =
  [ ("rimworld", "294100")
  , ("hoi4", "394360")
  , ("hearts of iron 4", "394360")
  ]

myTerminal :: String
myTerminal = "terminator"

myProjects :: [Project]
myProjects =
  [ gaming
  , casper
  ]
  where
    casper = Project
      { projectName = "casper"
      , projectDirectory = "~/Documents/GitHub/SamuelSchlesinger/casper-node"
      , projectStartHook = Just $ do
          spawn myTerminal
      } 
    gaming = Project
      { projectName = "gaming"
      , projectDirectory = "~"
      , projectStartHook = Just $ do
          spawn "steam"
      }

myTheme = defaultThemeWithImageButtons
