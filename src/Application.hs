{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}

module Application
    ( appMain
    ) where

import Foundation
import Yesod
import Handler

mkYesodDispatch "App" resourcesApp

mkFoundation = undefined

develMain = undefined

appMain = warp 3000 App
