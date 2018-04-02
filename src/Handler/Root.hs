{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root
    ( getRootR
    ) where

import Text.Blaze.Html (Html)
import Foundation
import Yesod
import Yesod.Auth
import Yesod.Core.Widget (whamlet)
import Yesod.Core (defaultLayout)

getRootR :: Handler Html
getRootR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]
