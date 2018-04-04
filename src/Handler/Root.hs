{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root
    ( getRootR
    ) where

import           Foundation
import           Text.Blaze.Html (Html)
import           Yesod.Core.Widget (whamlet)
import           Yesod.Core (defaultLayout)

getRootR :: Handler Html
getRootR = do
    defaultLayout
        [whamlet|
            <h1> WELCOME TO THE SHOW!
            <p> Not implemented
        |]
