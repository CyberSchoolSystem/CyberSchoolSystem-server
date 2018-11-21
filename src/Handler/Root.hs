{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handler.Root
    ( getRootR
    -- , getFaviconR
    ) where

import           Foundation
import           Data.Monoid        ((<>))
import           Settings           (appReload)
import           Text.Blaze.Html    (Html)
import           Text.Julius        (juliusFile, juliusFileReload)
import           Text.Hamlet        (hamletFile, hamletFileReload)
import           Yesod.Core         (defaultLayout)
import           Yesod.Core.Handler (getYesod)
import           Yesod.Core.Widget  (toWidget, setTitle)

getRootR :: Handler Html
getRootR = defaultLayout $ do
    setTitle "Startseite"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/root.julius")
             <> toWidget $(hamletFileReload "templates/root.hamlet")
        else toWidget $(juliusFile "templates/root.julius")
             <> toWidget $(hamletFile "templates/root.hamlet")

-- getFaviconR :: Handler TypedContent
-- getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache a month
--                  return $ TypedContent "image/x-icon"
--                         $ toContent $(embedFile "config/favicon.ico")
