{-# LANGUAGE RecursiveDo #-}
module Game(
    runGame
  ) where

import Game.Config
import Game.Map
import Game.Monad
import Game.Player
import Game.TCOD

runGame :: IO ()
runGame = do
  e <- newEnv Config
  runGameM e $ mdo
    let
      mw = 40
      mh = 40
    tickE <- tickEvery 0.5
    PlayerOutputs{..} <- runPlayer PlayerConfig {
        playerMaxHunger = 10
      , playerFoodEaten = mapPlayerConsume
      , playerInitPosition = (mw `div` 2, mh `div` 2)
      , playerInitRotation = DirUp
      , playerPosValidate = mapPlayerPosValidate
      , playerBlocked = mapPlayerBlocked
      , playerFoodSense = mapPlayerSensed
      , playerTurn = tickE
      }
    MapOutputs{..} <- runMap MapConfig {
        mapWidth = mw
      , mapHeight = mh
      , mapOffsetX = 5
      , mapOffsetY = 5
      , mapFoodCount = 140
      , mapFoodDensity = 0.7
      , mapPlayerPos = playerPosition
      , mapPlayerRot = playerRotation
      , mapPlayerDead = playerDead
      , mapFoodSaturation = 5
      }
    let labelsX = 48
        labelsW = 5
    displayCounter "Food" labelsX 5 labelsW mapFoodLeft
    displayCounter "Eaten" labelsX 7 labelsW mapPlayerFood
    displayCounter "Hunger" labelsX 9 labelsW playerHunger
    displayCounter "Position" labelsX 11 labelsW playerPosition
    displayCounter "Rotation" labelsX 13 labelsW playerRotation
    displayCounter "Sense" labelsX 15 labelsW mapPlayerSensed
    displayCounter "Blocked" labelsX 17 labelsW mapPlayerBlocked
    displayCounter "Action" labelsX 19 labelsW playerAction

-- | Display counter with label at given pos
displayCounter :: (MonadGame t m, Show a) => String -> Int -> Int -> Int -> Dynamic t a -> m ()
displayCounter label x y w d = do
  e <- updatedWithInit d
  performEvent_ $ ffor e $ \v -> liftIO $
    consolePrint rootConsole x y (label <> ": %-" <> show w <> "s") $ show v
