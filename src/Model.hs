{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Model where

import Data.ByteString            (ByteString)
import Database.Persist.MongoDB
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Text                  (Text)
import Language.Haskell.TH.Syntax (Type (ConT))

type ID = Int
type Votes = Int
type Choice = [(Text, (ID, Votes))] -- TODO: Deuglify

let mongoSettings = mkPersistSettings (ConT ''MongoContext) -- TODO: Default Settings?!?!
 in share [mkPersist mongoSettings] $(persistFileWith upperCaseSettings "config/models")
