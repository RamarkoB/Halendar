{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import DateTime
import Miso
import Model
import Update
import View

#ifdef WASM
-- WebAssembly version - no server needed
main :: IO ()
main = do
  css <- readFile "cal.css"
  let env = Env css

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

-- WASM export, required when compiling w/ the WASM backend.
foreign export javascript "hs_start" main :: IO ()

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