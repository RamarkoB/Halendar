{-# LANGUAGE RecordWildCards #-}

module Main where

import DateTime
import Miso
import Miso.String (MisoString, ms)
import Model
import Update
import View

-- For GHCJS, we don't need jsaddle-warp
main :: IO ()
main = do
  -- You'll need to embed the CSS or load it differently for static deployment
  let css = "" -- You can embed your CSS content here as a string
  let env = Env css

  t <- today
  n <- rightNow

  startApp
    App
      { initialAction = OnLoad,
        model = initialModel t n,
        update = updateModel,
        view = viewModel env,
        events = defaultEvents,
        subs = [keyboardSub keysToAction, arrowsToAction, every 60000 TimeUpdate],
        mountPoint = Nothing,
        logLevel = Off
      }