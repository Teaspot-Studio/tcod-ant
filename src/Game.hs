module Game(
    runGame
  ) where

import Game.Config
import Game.Map
import Game.Monad

runGame :: IO ()
runGame = do
  e <- newEnv Config
  runGameM e $ do
    runMap MapConfig {
        mapWidth = 40
      , mapHeight = 40
      , mapOffsetX = 5
      , mapOffsetY = 5
      , mapFoodCount = 70
      , mapFoodDensity = 0.7
      }
