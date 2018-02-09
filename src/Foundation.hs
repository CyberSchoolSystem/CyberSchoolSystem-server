{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation where

import Yesod

data App = App

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App
