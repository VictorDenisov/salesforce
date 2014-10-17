module Salesforce.Utils
( deriveJson
) where

import Data.Char (isUpper, toLower)
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
customModifier = (map toLower) . (intercalate "_") . splitCamel
  where splitCamel = split (keepDelimsL $ whenElt isUpper)
