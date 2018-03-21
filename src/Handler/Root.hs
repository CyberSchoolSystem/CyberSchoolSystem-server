{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Handler.Root
    ( getRootR
    ) where

import Text.Blaze.Html (Html)
import Foundation
import Yesod.Core.Widget (whamlet)
import Yesod.Core (defaultLayout)

getRootR :: Handler Html
getRootR = defaultLayout [whamlet| <h1> UI not implemented yet.|]
