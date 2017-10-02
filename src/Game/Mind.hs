module Game.Mind(
    Direction(..)
  , applyDirection
  , PlayerRotation(..)
  , applyRotation
  , PlayerAction(..)
  , MindInput(..)
  , mind
  ) where

import Game.Monad

-- | Possible directions of facing for player
data Direction = DirUp | DirRight | DirDown | DirLeft
  deriving (Eq, Ord, Show)

-- | Get position at the given direction relative to the point
applyDirection :: Direction -> (Int, Int) -> (Int, Int)
applyDirection d (x, y) = case d of
  DirUp -> (x, y+1)
  DirRight -> (x+1, y)
  DirDown -> (x, y-1)
  DirLeft -> (x-1, y)

-- | Which rotation changes player can do
data PlayerRotation = PlayerRotLeft | PlayerRotRight
  deriving (Eq, Ord, Show)

-- | Rotate direction according to player rotation change
applyRotation :: PlayerRotation -> Direction -> Direction
applyRotation pr d = case pr of
  PlayerRotLeft -> case d of
    DirUp -> DirLeft
    DirRight -> DirUp
    DirDown -> DirRight
    DirLeft -> DirDown
  PlayerRotRight -> case d of
    DirUp -> DirRight
    DirRight -> DirDown
    DirDown -> DirLeft
    DirLeft -> DirUp

-- | Possible actions that player can do
data PlayerAction = PlayerMove | PlayerRot PlayerRotation | PlayerDoNothing
  deriving (Eq, Ord, Show)

-- | Which data is avaliable for player for decision making
data MindInput t = MindInput {
  mindBlocked    :: Dynamic t Bool -- ^ Cannot move forward
, mindFoodSense  :: Dynamic t Bool -- ^ Food is in front of body
, mindHunger     :: Dynamic t Int -- ^ Hunger level
, mindFoodEaten  :: Event t Int -- ^ Food consumed
, mindLastAction :: Event t PlayerAction -- ^ Last player action
}

-- | Peform decision making, the resulted dynamic is sampled at simulation
-- speed to make turn based execution.
mind :: MonadGame t m => MindInput t -> m (Dynamic t PlayerAction)
mind MindInput{..} = foldDynM genRandAction PlayerDoNothing mindLastAction
  where
    genRandAction _ _ = liftIO $ uniform $ PlayerMove : (PlayerRot <$> [PlayerRotLeft, PlayerRotRight])
