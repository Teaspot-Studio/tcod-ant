module Game(
    runGame
  ) where

import Game.Config
import Game.Monad
import Game.TCOD

runGame :: IO ()
runGame = do
  e <- newEnv Config
  runGameM e $ pure ()
