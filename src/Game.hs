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
    roundEndE <- switchPromptlyDyn <$> holdAppHost (playRound 0) nextRoundE
    roundEndDelayedE <- delayBy 0.1 roundEndE
    cleanupE <- performEvent $ ffor roundEndDelayedE $ \n -> do
      liftIO $ consoleClear rootConsole
      pure n
    let nextRoundE = playRound . (+1) <$> cleanupE
    pure ()

-- | Play single round of game, fire when it ends with number of round
playRound :: (MonadGame t m) => Int -> m (Event t Int)
playRound roundNumber = mdo
  let
    mw = 40
    mh = 40
  tickE <- tickEvery 0.5
  PlayerOutputs{..} <- runPlayer PlayerConfig {
      playerMaxHunger = 30
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
  displayCounter "Round" labelsX 5 labelsW $ pure roundNumber
  displayCounter "Food" labelsX 7 labelsW mapFoodLeft
  displayCounter "Eaten" labelsX 9 labelsW mapPlayerFood
  displayCounter "Hunger" labelsX 11 labelsW playerHunger
  displayCounter "Position" labelsX 13 labelsW playerPosition
  displayCounter "Rotation" labelsX 15 labelsW playerRotation
  displayCounter "Sense" labelsX 17 labelsW mapPlayerSensed
  displayCounter "Blocked" labelsX 19 labelsW mapPlayerBlocked
  displayCounter "Action" labelsX 21 labelsW playerAction
  headE $ fmap (const roundNumber) $ ffilter id $ updated playerDead

-- | Display counter with label at given pos
displayCounter :: (MonadGame t m, Show a) => String -> Int -> Int -> Int -> Dynamic t a -> m ()
displayCounter label x y w d = do
  e <- updatedWithInit d
  performEvent_ $ ffor e $ \v -> liftIO $
    consolePrint rootConsole x y (label <> ": %-" <> show w <> "s") $ show v
