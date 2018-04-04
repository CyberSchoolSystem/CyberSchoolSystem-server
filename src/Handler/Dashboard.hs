{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getDashboardR
    , getUiUserAddR
    ) where

import           Foundation
import           Text.Blaze.Html (Html)
import           Yesod.Core (defaultLayout)
import           Yesod.Core.Widget (whamlet)

getUiUserAddR :: Handler Html
getUiUserAddR = undefined

getDashboardR :: Handler Html
getDashboardR = defaultLayout
    [whamlet|
        <h1> Citizen Stuff
        <ul>
            <li> Votes
        <h1> Rep Stuff
        <ul>
            <li> Add Votes
        <h1> Teacher stuff
        <ul>
            <li> Access times
        <h1> Admin Stuff
        <ul> 
            <li> <a href=@{UiUserAddR}
    |]
