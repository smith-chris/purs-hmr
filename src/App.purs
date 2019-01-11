module App where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

main :: forall a b. MonadEffect b => a -> b Unit
main time = do
  log "Hello World!!!"