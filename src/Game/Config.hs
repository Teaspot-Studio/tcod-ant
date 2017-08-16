module Game.Config(
    Config(..)
  , readConfig
  ) where

import Data.Aeson.TH
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Data.Yaml.Config

-- | Game configuration
data Config = Config {

}
deriveJSON defaultOptions ''Config

-- | Read config from file and use environment variables
readConfig :: MonadIO m => FilePath -> m Config
readConfig f = liftIO $ loadYamlSettings [f] [] useEnv
