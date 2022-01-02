-- XMonad imports
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as StackSet

import XMonad.Prompt (XPrompt(..), mkComplFunFromList, mkXPrompt, XPConfig(..))
import XMonad.Prompt.Man
import XMonad.Prompt.XMonad

import XMonad.Config.Desktop (desktopConfig)

import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)

import XMonad.Actions.DynamicProjects (switchProjectPrompt, Project(..), dynamicProjects, switchProject, shiftToProjectPrompt)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt)

import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Dwindle
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)


-- Base imports
import System.IO

main :: IO ()
main = do
    (xmonad =<<) . xmobar . dynamicProjects myProjects $ desktopConfig
      { terminal = myTerminal
      , manageHook =
          manageDocks
          <+> manageHook desktopConfig
          <+> (isFullscreen --> doFullFloat)
      , workspaces = map projectName myProjects
      , layoutHook =
          avoidStruts
          $ (   spacing 10 (GridRatio (3/2))
            ||| tabbed shrinkText myTheme
            ||| spacing 10 (Tall { tallNMaster = 1, tallRatioIncrement = 3/100, tallRatio = 2/3 })
            )
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
            (\domain -> safeSpawn "firefox" ["https://" <> domain])
        )
      , ((mod4Mask .|. shiftMask, xK_s),
          mkXPrompt
            SteamGame
            xpromptConfig
            (mkComplFunFromList (map fst mySteamGames))
          (\x -> case lookup x mySteamGames of
            Just steamId -> safeSpawn "steam" ["steam://rungameid/" <> steamId]
            Nothing -> safeSpawn "steam" []
          )
        )
      , ((mod4Mask .|. shiftMask, xK_h),
          safeSpawn "firefox" ["~/Documents/xmonad-keybindings-cheatsheet.png"]
        )
      , ((mod4Mask .|. shiftMask, xK_l),
          addWorkspacePrompt xpromptConfig
        )
      , ((mod4Mask .|. shiftMask, xK_p),
          switchProjectPrompt xpromptConfig
        )
      , ((mod4Mask .|. shiftMask, xK_o),
          shiftToProjectPrompt xpromptConfig
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
  { font = "xft:Monospace:pixelsize=20"
  , height = 30
  , showCompletionOnTab = False
  , bgColor = "grey"
  , fgColor = "darkblue"
  , bgHLight = "black"
  , fgHLight = "blue"
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
myTerminal = "alacritty"

myProjects :: [Project]
myProjects =
  [ defaultProject
  , discord
  , browser
  , development
  , steam
  , casper
  , biomassBreakout
  , xmonad
  , nordpass
  , cluster
  ]
  where
    defaultProject = Project
      { projectName = "default"
      , projectDirectory = "~"
      , projectStartHook = Just $ do
          safeSpawn "nordvpn" ["connect"]
      }
    discord = Project
      { projectName = "discord"
      , projectDirectory = "~/.Discord"
      , projectStartHook = Just $ do
          spawn "~/.Discord/Discord"
      }
    browser = Project
      { projectName = "browser"
      , projectDirectory = "~/.xmonad"
      , projectStartHook = Just $ do
          safeSpawn "firefox" []
      }
    xmonad = Project
      { projectName = "xmonad"
      , projectDirectory = "~/.xmonad"
      , projectStartHook = Just $ do
          safeSpawn myTerminal ["-e", "vim xmonad.hs"]
      }
    development = Project
      { projectName = "coding"
      , projectDirectory = "~/Documents/GitHub/SamuelSchlesinger"
      , projectStartHook = Just $ do
          safeSpawn myTerminal []
      }
    casper = Project
      { projectName = "casper"
      , projectDirectory = "~/Documents/GitHub/SamuelSchlesinger/casper-node"
      , projectStartHook = Just $ do
          safeSpawn myTerminal []
      } 
    steam = Project
      { projectName = "steam"
      , projectDirectory = "~"
      , projectStartHook = Just $ do
          safeSpawn "steam" []
      }
    biomassBreakout = Project
      { projectName = "biomass-breakout"
      , projectDirectory = "~/Documents/GitHub/SamuelSchlesinger/biomass-breakout"
      , projectStartHook = Just $ do
          safeSpawn "firefox" ["https://docs.rs/glium"]
          safeSpawn "firefox" ["https://docs.rs/glutin"]
          safeSpawn myTerminal ["-e", "vim -c \":GFiles\""]
      }
    nordpass = Project
      { projectName = "nordpass"
      , projectDirectory = "~"
      , projectStartHook = Just $ safeSpawn "nordpass" []
      }
    vimrc = Project
      { projectName = "vimrc"
      , projectDirectory = "~"
      , projectStartHook = Just $ safeSpawn "vim" ["~/.vimrc"]
      }
    cluster = Project
      { projectName = "cluster"
      , projectDirectory = "~/Documents/GitHub/SamuelSchlesinger/cluster"
      , projectStartHook = Just $ safeSpawn myTerminal []
      }

myTheme = defaultThemeWithImageButtons
