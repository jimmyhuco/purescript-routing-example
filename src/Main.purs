module Main where

import BigPrelude

import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as R

main :: forall eff. Eff (HA.HalogenEffects (console :: CONSOLE | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI R.ui unit body
  _ <- forkAff $ R.routeSignal driver
  forkAff $ log "fork log in main"
