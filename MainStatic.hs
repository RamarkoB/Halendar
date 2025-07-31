{-# LANGUAGE RecordWildCards #-}

module Main where

import DateTime
import Miso
import Model
import Update
import View

-- Static version for GHCJS compilation - no JSaddle needed
main :: IO ()
main = do
  -- CSS will be loaded via link tag in HTML, so empty string here
  let css = ""
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
        mountPoint = Nothing, -- mount point (Nothing defaults to 'body')
        logLevel = Off -- used during prerendering to if VDOM/DOM in sync
      }