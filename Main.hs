{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import DateTime
import Miso
import Model
import Update
import View

#ifdef GHCJS
-- GHCJS version - no server needed
main :: IO ()
main = do
  -- In GHCJS builds, CSS is loaded via <link> in index.html
  -- We can't read from filesystem in the browser
  let env = Env ""  -- Empty CSS string, CSS provided via <link> in index.html

  t <- today
  n <- rightNow

  startApp App
    { initialAction = OnLoad
    , model         = initialModel t n
    , update        = updateModel
    , view          = viewModel env
    , events        = defaultEvents
    , subs          = [keyboardSub keysToAction, arrowsSub arrowsToAction, every 60000 TimeUpdate]
    , mountPoint    = Nothing
    , logLevel      = Off
    }

-- GHCJS export, required when compiling w/ GHCJS.
foreign export javascript "main" main :: IO ()

#else
-- Development version with jsaddle-warp server
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp =
  JSaddle.run 8000
  -- https://hackage.haskell.org/package/jsaddle-warp-0.9.8.2

--------------------------------------------------------------------------------

main :: IO ()
main = do
  css <- readFile "cal.css"
  let env = Env css

  t <- today
  n <- rightNow

  putStrLn ("Today is: " ++ show t)
  putStrLn "http://localhost:8000\n"

  runApp $ startApp App
    { initialAction = OnLoad
    , model         = initialModel t n
    , update        = updateModel
    , view          = viewModel env
    , events        = defaultEvents
    , subs          = [keyboardSub keysToAction, arrowsSub arrowsToAction, every 60000 TimeUpdate]
    , mountPoint    = Nothing -- mount point (Nothing defaults to 'body')
    , logLevel      = Off     -- used during prerendering to if VDOM/DOM in sync
    }
#endif