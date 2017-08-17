{-# LANGUAGE RecursiveDo #-}
module Game.Player(
    Direction(..)
  , applyDirection
  , PlayerRotation(..)
  , applyRotation
  , PlayerAction(..)
  , PlayerConfig(..)
  , PlayerOutputs(..)
  , runPlayer
  ) where

import Game.Monad
import Game.TCOD
import Game.Mind

-- | Configuration for player component
data PlayerConfig t = PlayerConfig {
-- | Maximum number of steps, player can travel without food
  playerMaxHunger :: Int
-- | Player has eaten food with given saturation
, playerFoodEaten :: Event t Int
-- | Initial player position in world
, playerInitPosition :: (Int, Int)
-- | Initial player rotation in world
, playerInitRotation :: Direction
-- | Is movement forward is allowed?
, playerBlocked :: Dynamic t Bool
-- | Is food is in front of player?
, playerFoodSense :: Dynamic t Bool
-- | Fires when player is allowed to apply next action
, playerTurn :: Event t ()
}

-- | External events
data PlayerOutputs t = PlayerOutputs {
-- | Current player hunger level
  playerHunger :: Dynamic t Int
-- | Current player position in world
, playerPosition :: Dynamic t (Int, Int)
-- | Current player rotation in world
, playerRotation :: Dynamic t Direction
-- | Which action to perform next
, playerAction :: Dynamic t PlayerAction
-- | Is player is dead
, playerDead   :: Dynamic t Bool
}

-- | Reactimate player
runPlayer :: MonadGame t m => PlayerConfig t -> m (PlayerOutputs t)
runPlayer PlayerConfig{..} = mdo
  -- Calculate hunger
  let hungerChange = mergeWith (+) [playerFoodEaten, (-1) <$ playerTurn]
  hunger <- foldDyn addHunger playerMaxHunger hungerChange
  let dead = (<= 0) <$> hunger
  -- Calculage position
  let posNew = do
        a <- actionD
        case a of
          PlayerMove -> do
            isBlocked <- playerBlocked
            if isBlocked then pure Nothing else do
              p <- pos
              d <- rot
              pure $ Just $ applyDirection d p
          _ -> pure Nothing
  pos <- holdDyn playerInitPosition $ fmapMaybe id $ tagPromptlyDyn posNew playerTurn
  -- Calclulate rotation
  let rotNew = do
        a <- actionD
        case a of
          PlayerRot r -> do
            d <- rot
            pure $ Just $ applyRotation r d
          _ -> pure Nothing
  rot <- holdDyn playerInitRotation $ fmapMaybe id $ tagPromptlyDyn rotNew playerTurn
  -- Decision making
  mindAction <- mind MindInput {
      mindBlocked = playerBlocked
    , mindFoodSense = playerFoodSense
    , mindHunger = hunger
    , mindFoodEaten = playerFoodEaten
    }
  let actionD = do
        a <- mindAction
        isDead <- dead
        if isDead then pure PlayerDoNothing else pure a
  pure PlayerOutputs {
      playerHunger = hunger
    , playerPosition = pos
    , playerRotation = rot
    , playerAction = actionD
    , playerDead = dead
    }
  where
    addHunger v acc = max 0 $ min playerMaxHunger $ v + acc
    addPos (dx, dy) (x, y) = (x+dx, y+dy)
