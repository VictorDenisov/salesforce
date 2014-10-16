module Salesforce.Utils
( deriveJson
) where

import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.Split (split, whenElt, keepDelimsL)
import Language.Haskell.TH (Dec, Name, Q)
import qualified Data.Aeson.TH as J

deriveJson :: Name -> Q [Dec]
deriveJson = J.deriveJSON $
                      J.defaultOptions
                        { J.fieldLabelModifier = customModifier
                        }

customModifier :: String -> String
customModifier = (intercalate "_") . split (keepDelimsL $ whenElt isUpper)
