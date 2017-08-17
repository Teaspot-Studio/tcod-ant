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
import Debug.Trace

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
, mapFoodSaturation :: !Int
}

data MapOutputs t = MapOutputs {
  mapFoodLeft      :: Dynamic t Int -- ^ How much food is left on map
, mapPlayerFood    :: Dynamic t Int -- ^ How much food is consumed by player
, mapPlayerSensed  :: Dynamic t Bool -- ^ If player senses food in front of it
, mapPlayerConsume :: Event t Int -- ^ Fires when player steps on food
, mapPlayerBlocked :: Dynamic t Bool -- ^ If player cannot move forward
, mapPlayerPosValidate :: (Int, Int) -> Bool -- ^ Validates player position in world
}

-- | Level widget
runMap :: MonadGame t m => MapConfig t -> m (MapOutputs t)
runMap cfg@MapConfig{..} = do
  buildE <- getPostBuild
  let cx = mapWidth `div` 2
      cy = mapHeight `div` 2
  prevPlayerPos <- dynamicDelay (cx, cy) clampPlayerPos
  -- fov <- liftIO $ mapNew mapWidth mapHeight
  (foodCount0, foodArr) <- makeFoodArray cfg
  render $ ffor buildE $ const $ do
    consolePrintFrame' rootConsole (mapOffsetX-1) (mapOffsetY-1) (mapWidth+2) (mapHeight+2) False BackgroundDefault
    printFoodArray cfg foodArr

  -- consume food under player pos
  updPosE <- updatedWithInit clampPlayerPos
  consumeE <- fmap (fmapMaybe id) $ performEvent $ ffor updPosE $ \p -> do
    v <- liftIO $ readFood cfg foodArr p
    when v $ liftIO $ writeFood cfg foodArr p False
    pure $ if v then Just mapFoodSaturation else Nothing

  -- render player position
  let renderInfo = (,,,) <$> clampPlayerPos <*> prevPlayerPos <*> mapPlayerRot <*> mapPlayerDead
  renderE <- updatedWithInit renderInfo
  render $ ffor renderE $ \((px, py), (opx, opy), dir, dead) -> do
    v <- readFood cfg foodArr (opx, opy)
    printMapChar cfg opx opy $ if v then '.' else ' '
    printMapChar cfg px py $ if dead then 'x' else case dir of
      DirUp -> '^'
      DirRight -> '>'
      DirLeft -> '<'
      DirDown -> 'v'

  -- detect food in front of player
  frontCellE <- updatedWithInit clampForwardPoint
  senseE <- performEvent $ ffor frontCellE $ liftIO . readFood cfg foodArr
  foodSensed <- holdDyn False senseE

  -- count foood left and food consumed
  let eatenE = (`div` mapFoodSaturation) <$> consumeE
  foodCount <- foldDyn (flip (-)) foodCount0 eatenE
  consumedCount <- foldDyn (+) 0 eatenE
  -- Output data
  pure MapOutputs {
      mapFoodLeft = foodCount
    , mapPlayerFood = consumedCount
    , mapPlayerSensed = foodSensed
    , mapPlayerConsume = consumeE
    , mapPlayerBlocked = do
        (x, y) <- playerForwardPoint
        pure $ x < 0 || x >= mapWidth || y < 0 || y >= mapHeight
    , mapPlayerPosValidate = \(x, y) -> x >= 0 && y < mapWidth && y >= 0 && y < mapHeight
    }
  where
    clampPos (x, y) = (max 0 . min (mapWidth-1) $ x, max 0 . min (mapHeight-1) $ y)
    clampPlayerPos = clampPos <$> mapPlayerPos
    playerForwardPoint = applyDirection <$> mapPlayerRot <*> clampPlayerPos
    clampForwardPoint = fmap clampPos playerForwardPoint

-- | Put given char on map
printMapChar :: MapConfig t -> Int -> Int -> Char -> IO ()
printMapChar MapConfig{..} x y c = consolePutChar rootConsole (x + mapOffsetX) (mapHeight - y - 1 + mapOffsetY) c BackgroundDefault

-- | Check that food map has food on given location
readFood ::  MapConfig t -> IOUArray (Int, Int) Bool -> (Int, Int) -> IO Bool
readFood MapConfig{..} arr (x, y) = readArray arr (max 0 . min (mapWidth-1) $ x, max 0 . min (mapHeight-1) $ y)

-- | Check that food map has food on given location
writeFood ::  MapConfig t -> IOUArray (Int, Int) Bool -> (Int, Int) -> Bool -> IO ()
writeFood MapConfig{..} arr (x, y) = writeArray arr (max 0 . min (mapWidth-1) $ x, max 0 . min (mapHeight-1) $ y)

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
