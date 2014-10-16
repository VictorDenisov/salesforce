{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Salesforce.Token
( Token(..)
) where

import Prelude hiding (id)
import Salesforce.Utils (deriveJson)

data Token = Token { id          :: String
                   , issuedAt    :: String
                   , tokenType   :: String
                   , instanceUrl :: String
                   , signature   :: String
                   , accessToken :: String
                   } deriving (Show)

deriveJson ''Token
