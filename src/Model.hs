{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}

module Model where

import Data.ByteString            (ByteString)
import Data.Time.Clock            (UTCTime)
import Data.Text                  (Text)
import Database.Persist.MongoDB
import Database.Persist.Quasi
import Database.Persist.TH
import Language.Haskell.TH.Syntax (Type (ConT))


let mongoSettings = mkPersistSettings (ConT ''MongoContext) -- TODO: Default Settings?!?!
 in share [mkPersist mongoSettings] $(persistFileWith upperCaseSettings "config/models")


instance Ord Access where
    compare x y = accessTime x `compare` accessTime y
    x <= y      = accessTime x <= accessTime y
