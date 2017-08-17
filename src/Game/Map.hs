module Game.Map(
    MapConfig(..)
  , MapOutputs(..)
  , runMap
  ) where

import Control.Monad.List
import Data.Array.IO
import Game.Monad
import Game.Player
import Game.TCOD

data MapConfig t = MapConfig {
  mapWidth       :: !Int
, mapHeight      :: !Int
, mapOffsetX     :: !Int
, mapOffsetY     :: !Int
, mapFoodCount   :: !Int
, mapFoodDensity :: !Rational
, mapPlayerPos   :: Dynamic t (Int, Int)
, mapPlayerRot   :: Dynamic t Direction
, mapPlayerDead  :: Dynamic t Bool
}

data MapOutputs t = MapOutputs {
  mapFoodLeft      :: Dynamic t Int -- ^ How much food is left on map
, mapPlayerFood    :: Dynamic t Int -- ^ How much food is consumed by player
, mapPlayerSensed  :: Dynamic t Bool -- ^ If player senses food in front of it
, mapPlayerConsume :: Event t Int -- ^ Fires when player steps on food
, mapPlayerBlocked :: Dynamic t Bool -- ^ If player cannot move forward
}

-- | Level widget
runMap :: MonadGame t m => MapConfig t -> m (MapOutputs t)
runMap cfg@MapConfig{..} = do
  buildE <- getPostBuild
  let cx = mapWidth `div` 2
      cy = mapHeight `div` 2
  prevPlayerPos <- dynamicDelay (cx, cy) mapPlayerPos
  -- fov <- liftIO $ mapNew mapWidth mapHeight
  (foodCount0, foodArr) <- makeFoodArray cfg
  render $ ffor buildE $ const $ do
    consolePrintFrame' rootConsole (mapOffsetX-1) (mapOffsetY-1) (mapWidth+2) (mapHeight+2) False BackgroundDefault
    printFoodArray cfg foodArr

  -- consume food under player pos
  updPosE <- updatedWithInit mapPlayerPos
  consumeE <- fmap (fmapMaybe id) $ performEvent $ ffor updPosE $ \p -> do
    v <- liftIO $ readArray foodArr p
    when v $ liftIO $ writeArray foodArr p False
    pure $ if v then Just 1 else Nothing

  -- render player position
  let renderInfo = (,,,) <$> mapPlayerPos <*> prevPlayerPos <*> mapPlayerRot <*> mapPlayerDead
  renderE <- updatedWithInit renderInfo
  render $ ffor renderE $ \((px, py), (opx, opy), dir, dead) -> do
    v <- readArray foodArr (opx, opy)
    when v $ printMapChar cfg opx opy '.'
    printMapChar cfg px py $ if dead then 'x' else case dir of
      DirUp -> '^'
      DirRight -> '>'
      DirLeft -> '<'
      DirDown -> 'v'

  -- detect food in front of player
  let playerForwardPoint = applyDirection <$> mapPlayerRot <*> mapPlayerPos
  frontCellE <- updatedWithInit playerForwardPoint
  senseE <- performEvent $ ffor frontCellE $ liftIO . readArray foodArr
  foodSensed <- holdDyn False senseE

  -- count foood left and food consumed
  foodCount <- foldDyn (flip (-)) foodCount0 consumeE
  consumedCount <- foldDyn (+) 0 consumeE
  -- Output data
  pure MapOutputs {
      mapFoodLeft = foodCount
    , mapPlayerFood = consumedCount
    , mapPlayerSensed = foodSensed
    , mapPlayerConsume = consumeE
    , mapPlayerBlocked = do
        (x, y) <- playerForwardPoint
        pure $ x < 0 || x >= mapWidth || y < 0 || y >= mapHeight
    }

-- | Put given char on map
printMapChar :: MapConfig t -> Int -> Int -> Char -> IO ()
printMapChar MapConfig{..} x y c = consolePutChar rootConsole (x + mapOffsetX) (mapHeight - y - 1 + mapOffsetY) c BackgroundDefault

-- | Render food array
printFoodArray :: MapConfig t -> IOUArray (Int, Int) Bool -> IO ()
printFoodArray cfg@MapConfig{..} arr = forM_ ps $ \p@(x, y) -> do
  v <- readArray arr p
  when v $ printMapChar cfg x y '.'
  where
    ps = [(x, y)|
          x <- [0 .. mapWidth-1]
        , y <- [0 .. mapHeight-1]
        ]

-- | Make 2D array that defines where to place food (make route from food)
makeFoodArray :: MonadGame t m => MapConfig t -> m (Int, IOUArray (Int, Int) Bool)
makeFoodArray MapConfig{..} = do
  arr <- liftIO $ newArray ((0, 0),(mapWidth, mapHeight)) False
  let cx = mapWidth `div` 2
      cy = mapHeight `div` 2
  n <- go arr cx cy cx cy 0 False
  pure (n, arr)
  where
    go arr !ox !oy !x !y !n !wasGap
      | n >= mapFoodCount = pure n
      | otherwise = do
        v <- if wasGap then pure True else weighted [(True, mapFoodDensity), (False, 1-mapFoodDensity)]
        liftIO $ writeArray arr (x, y) v
        moves <- runListT $ do
          nx <- ListT $ pure [x-1 .. x+1]
          ny <- ListT $ pure [y-1 .. y+1]
          guard (nx >= 0 && nx < mapWidth && nx /= ox)
          guard (ny >= 0 && ny < mapHeight && ny /= oy)
          v <- liftIO $ readArray arr (nx, ny)
          guard (not v)
          pure (nx, ny)
        if null moves then pure n else do
          (nx, ny) <- uniform moves
          let n' = if v then n+1 else n
              wasGap' = not v
          go arr x y nx ny n' wasGap'
