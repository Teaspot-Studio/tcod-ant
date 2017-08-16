module Game.Map(
    MapConfig(..)
  , runMap
  ) where

import Control.Monad.List
import Data.Array.IO
import Game.Monad
import Game.TCOD

data MapConfig t = MapConfig {
  mapWidth       :: !Int
, mapHeight      :: !Int
, mapOffsetX     :: !Int
, mapOffsetY     :: !Int
, mapFoodCount   :: !Int
, mapFoodDensity :: !Rational
}

-- | Level widget
runMap :: MonadGame t m => MapConfig t -> m ()
runMap cfg@MapConfig{..} = do
  buildE <- getPostBuild
  -- fov <- liftIO $ mapNew mapWidth mapHeight
  foodArr <- makeFoodArray cfg
  render $ ffor buildE $ const $ do
    consolePrintFrame' rootConsole (mapOffsetX-1) (mapOffsetY-1) (mapWidth+2) (mapHeight+2) False BackgroundDefault
    printFoodArray cfg foodArr

-- | Render food array
printFoodArray :: MapConfig t -> IOUArray (Int, Int) Bool -> IO ()
printFoodArray MapConfig{..} arr = forM_ ps $ \p@(x, y) -> do
  v <- readArray arr p
  when v $ consolePutChar rootConsole (x + mapOffsetX) (y + mapOffsetY) '.' BackgroundDefault
  where
    ps = [(x, y)|
          x <- [0 .. mapWidth-1]
        , y <- [0 .. mapHeight-1]
        ]

-- | Make 2D array that defines where to place food (make route from food)
makeFoodArray :: MonadGame t m => MapConfig t -> m (IOUArray (Int, Int) Bool)
makeFoodArray MapConfig{..} = do
  arr <- liftIO $ newArray ((0, 0),(mapWidth, mapHeight)) False
  let cx = mapWidth `div` 2
      cy = mapHeight `div` 2
  go arr cx cy cx cy 0 False
  pure arr
  where
    go arr !ox !oy !x !y !n !wasGap
      | n >= mapFoodCount = pure ()
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
        if null moves then pure () else do
          (nx, ny) <- uniform moves
          let n' = if v then n+1 else n
              wasGap' = not v
          go arr x y nx ny n' wasGap'
