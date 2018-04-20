{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handler.Root
    ( getRootR
    , getFaviconR
    ) where

import           Data.FileEmbed     (embedFile)
import           Foundation
import           Text.Blaze.Html    (Html)
import           Yesod.Core         (defaultLayout)
import           Yesod.Core.Content (TypedContent(..), toContent)
import           Yesod.Core.Handler (cacheSeconds)
import           Yesod.Core.Widget  (whamlet)

getRootR :: Handler Html
getRootR = do
    defaultLayout
        [whamlet|
            <h1> WELCOME TO THE SHOW!
            <p> Not implemented
        |]

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")
