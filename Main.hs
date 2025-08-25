{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#ifdef WASM
import Miso
#else
import Language.Javascript.JSaddle.Warp as JSaddle
import Miso
#endif

import DateTime
import Model
import View
import Update

#ifndef WASM
runApp :: JSM () -> IO ()
runApp =
  JSaddle.run 8000
  -- https://hackage.haskell.org/package/jsaddle-warp-0.9.8.2
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
#ifdef WASM
  let css = "" -- CSS will be loaded from external file in WASM build
#else
  css <- readFile "cal.css"
#endif
  let env = Env css

  t <- today
  n <- rightNow

  putStrLn ("Today is: " ++ show t)
#ifndef WASM
  putStrLn "http://localhost:8000\n"
#endif

#ifdef WASM
  startApp App
#else
  runApp $ startApp App
#endif
    { initialAction = OnLoad
    , model         = initialModel t n
    , update        = updateModel
    , view          = viewModel env
    , events        = defaultEvents
    , subs          = [keyboardSub keysToAction, arrowsSub arrowsToAction, every 60000 TimeUpdate]
    , mountPoint    = Nothing -- mount point (Nothing defaults to 'body')
    , logLevel      = Off     -- used during prerendering to if VDOM/DOM in sync
    }