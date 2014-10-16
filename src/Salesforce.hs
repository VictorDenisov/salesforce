module Salesforce
( Config(..)
) where

import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Conduit as H

data Config = Config { baseUrl :: String }

{-
data RuntimeState = RuntimeState
                      { config            :: SalesforceConfig
                      , connectionManager :: H.Manager
                      }

type SalesforceT m = ReaderT Config m

mkRuntimeState :: Config -> IO RuntimeState
mkRuntimeState config = do
  manager <- H.newManager (mkManagerSettings def Nothing)
  return $ RuntimeState config manager
  -}
