{-# LANGUAGE OverloadedLists #-}
module Game.Monad(
    MonadGame(..)
  , GameM
  , Env(..)
  , newEnv
  , runGameM
  , showt
  -- * Reexports of control primitives
  , MonadAppHost
  , AppInfo
  , AppHost
  , HostFrame
  , module Reflex
  , newExternalEvent
  , performEventAsync
  , performEventAndTrigger_
  , performEvent_
  , performEvent
  , switchAppHost
  , performAppHost
  , dynAppHost
  , holdAppHost
  , holdKeyAppHost
  , getPostBuild
  , performPostBuild
  ) where

import Control.Concurrent
import Control.Concurrent.Thread.Delay as TD
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Random.Strict
import Data.IORef
import Data.Text (Text, pack)
import Data.Time
import GHC.Event hiding (Event)
import GHC.Generics
import Reflex hiding (performEvent_, performEventAsync, performEvent, getPostBuild)
import Reflex.Host.App
import Reflex.Host.App.Internal
import Reflex.Host.Class
import Reflex.Spider

import qualified Data.Text.IO as T

import Game.Config
import Game.TCOD

-- | Helper to print anything to text
showt :: Show a => a -> Text
showt = pack . show

class (
    MonadHold t m
  , MonadSample t m
  , MonadFix m
  , MonadSubscribeEvent t m
  , MonadReflexCreateTrigger t m
  , MonadAppHost t m
  , MonadRandom m
  , MonadIO m
  , MonadReader Config m
  ) => MonadGame t m where

  -- | Get global configuration of server
  getConfig :: m Config

  -- | Put message to log immediately
  putLogStr :: Text -> m ()
  -- | Put message to log on event
  logStr :: Event t Text -> m ()

  -- | Access current time of server
  getTime :: m UTCTime

  -- | Get event that fires every n seconds.
  --
  -- Note: the event will tick even there are no subscriders to it. Use
  -- 'tickEveryUntil' if you want to free underlying thread.
  tickEvery :: NominalDiffTime -> m (Event t ())
  -- | Generate an event after given time once
  tickOnce :: NominalDiffTime -> m (Event t ())
  -- | Get event that fires every n seconds. Second event cease ticking.
  tickEveryUntil :: NominalDiffTime -> Event t a -> m (Event t ())
  -- | Get event that fires only after given event with specified delay
  delayBy :: NominalDiffTime -> Event t a -> m (Event t a)
  -- | Get event that fires only after given event with specified delay but
  -- not after the cancelation event.
  delayByUntil :: NominalDiffTime -> Event t a -> Event t b -> m (Event t b)

  -- | Signall to game that we need a shutdown
  gracefullExit :: Event t a -> m ()

newtype Env = Env {
  envConfig :: Config
}

newEnv :: MonadIO m => Config -> m Env
newEnv cfg =
  pure Env {
      envConfig = cfg
    }

-- | Internal monad that implements 'GameMonad' API
newtype GameM a = GameM { unGameM :: ReaderT Env (AppHost Spider) a }
  deriving (Functor, Applicative, Monad
    , MonadHold Spider
    , MonadSample Spider
    , MonadFix
    , MonadIO
    , MonadSubscribeEvent Spider
    , MonadReflexCreateTrigger Spider
    )

instance MonadRandom GameM where
  getRandomR = liftIO . getRandomR
  getRandom = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms = liftIO getRandoms
  {-# INLINE getRandomR #-}
  {-# INLINE getRandom #-}
  {-# INLINE getRandomRs #-}
  {-# INLINE getRandoms #-}

instance MonadReader Config GameM where
  ask = GameM $ asks envConfig
  local f (GameM ma) = GameM $ local f' ma
    where f' :: Env -> Env
          f' c = c { envConfig = f $ envConfig c }
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance MonadGame Spider GameM where
  getConfig = GameM $ asks envConfig
  {-# INLINE getConfig #-}
  getTime = liftIO getCurrentTime
  {-# INLINE getTime #-}
  putLogStr msg = liftIO . T.putStrLn $ msg
  {-# INLINE putLogStr #-}
  logStr e = performEvent_ $ liftIO . T.putStrLn <$> e
  {-# INLINE logStr #-}

  tickEvery t = do
    (tickEvent, fireTick) <- newExternalEvent
    _ <- ticker fireTick
    return tickEvent
    where
    ticker fire = liftIO $ do
      tm <- getSystemTimerManager
      let dt = ceiling $ (realToFrac t :: Double) * 1000000
          go = do
            _ <- fire ()
            void $ registerTimeout tm dt go
      registerTimeout tm dt go
  {-# INLINE tickEvery #-}

  tickOnce t = do
    (tickEvent, fireTick) <- newExternalEvent
    tm <- liftIO getSystemTimerManager
    let dt = ceiling $ (realToFrac t :: Double) * 1000000
    _ <- liftIO $ registerTimeout tm dt $ void $ fireTick ()
    return tickEvent
  {-# INLINE tickOnce #-}

  tickEveryUntil t ceaseE = do
    (tickEvent, fireTick) <- newExternalEvent
    stopRef <- liftIO $ newIORef False
    performEvent_ $ ffor ceaseE $ const $ liftIO $ writeIORef stopRef True
    _ <- ticker fireTick stopRef
    return tickEvent
    where
    ticker fire stopRef = liftIO $ do
      tm <- getSystemTimerManager
      let dt = ceiling $ (realToFrac t :: Double) * 1000000
          go = do
            _<- fire ()
            stop <- liftIO $ readIORef stopRef
            unless stop $ void $ registerTimeout tm dt go
      registerTimeout tm dt go
  {-# INLINE tickEveryUntil #-}

  delayBy t e = performEventAsync $ ffor e $ \a -> do
    TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
    return a
  {-# INLINE delayBy #-}

  delayByUntil t ce e = do
    stopRef <- liftIO $ newIORef False
    performEvent_ $ ffor ce $ const $ liftIO $ writeIORef stopRef True
    fmap (fmapMaybe id) $ performEventAsync $ ffor e $ \a -> do
      TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
      canceled <- liftIO $ readIORef stopRef
      pure $ if canceled then Nothing else Just a
  {-# INLINE delayByUntil #-}

  gracefullExit e = void $ switchAppHost (pure $ infoQuit [void e]) never
  {-# INLINE gracefullExit #-}

-- | Run the game monad, blocks until exit event occur.
runGameM :: Env -> GameM () -> IO ()
runGameM env m = withSystem $ runSpiderHost . hostApp . flip runReaderT env . unGameM $ do
  (exitE, exitFire) <- newExternalEvent
  _ <- liftIO $ forkOS $ do
    consoleInitRoot 80 50 "Ant problem" False RendererSDL
    unlessM consoleIsWindowClosed $ do
      _ <- systemCheckForEvent ([EventKeyPress] :: [TCODEvent])
      consoleClear rootConsole
      consolePutChar rootConsole 40 25 '@' BackgroundDefault
      consoleFlush
    _ <- exitFire ()
    pure ()
  gracefullExit exitE
  m

-- | Do action until given condition predicate doesn't return 'False'
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond ma = do
  cond <- mcond
  if not cond then do
    ma
    unlessM mcond ma
  else pure ()

instance MonadAppHost Spider GameM where
  getFireAsync = GameM $ lift getFireAsync
  {-# INLINE getFireAsync #-}
  getRunAppHost = GameM $ do
    runner <- lift getRunAppHost
    core <- ask
    pure $ runner . flip runReaderT core . unGameM
  {-# INLINE getRunAppHost #-}
  performPostBuild_ = GameM . lift . performPostBuild_
  {-# INLINE performPostBuild_ #-}
  liftHostFrame = GameM . lift . liftHostFrame
  {-# INLINE liftHostFrame #-}
