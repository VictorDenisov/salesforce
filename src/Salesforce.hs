module Salesforce
( Config(..)
) where

import Control.Monad.Reader (ReaderT)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit as H

data Config = Config { baseUrl :: String }

data RuntimeState = RuntimeState
                      { config            :: Config
                      , connectionManager :: H.Manager
                      }

type SalesforceT m = ReaderT Config m


mkRuntimeState :: Config -> IO RuntimeState
mkRuntimeState config = do
  manager <- H.newManager tlsManagerSettings
  return $ RuntimeState config manager
