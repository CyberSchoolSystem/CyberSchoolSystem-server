module Handler.File
    ( getFileR
    , addFile
    ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime())
import           Database.Persist       ((==.))
import           Database.Persist.Class (selectFirst, insert_)
import           Database.Persist.Types (Entity(..))
import           Foundation
import           Model
import           Settings
import           Yesod.Auth             (requireAuthId)
import           Yesod.Core.Content     (toContent)
import           Yesod.Core.Handler     (notFound, getYesod)
import           Yesod.Core.Types       (TypedContent(), TypedContent(..), ContentType())
import           Yesod.Persist.Core     (runDB)

{-| Get a file if it belongs to the correct user -}
getFileR :: Text -> Handler TypedContent
getFileR title = do
    auth <- requireAuthId
    user <- runDB $ selectFirst [UserId ==. auth] []
    file <- runDB $ selectFirst [FileTitle ==. title, FileOwner ==. auth] []
    case (user, file) of
        (Just _, Just f) -> do
            let val = entityVal f
                mime = fileMime val
            cont <- liftIO $ toContent <$> BS.readFile (fileFilePath val)
            return $ TypedContent mime cont
        (Just _, Nothing) -> notFound
        (Nothing, _) -> error "User not in database. This should not happen"

{-| Add a file and check for free filenames -}
addFile :: UserId -> ContentType -> UTCTime -> Text -> Handler (Maybe File)
addFile u mime destroy title = do
    app <- getYesod
    test <- runDB $ selectFirst [FileTitle ==. title] []
    case test of
        Nothing -> do
            let path = (appTmpFilePath . appSettings $ app)
                file = File{ fileFilePath = path <> "/" <> show title, fileTitle = title,
                             fileOwner = u, fileDestroy = destroy, fileMime = mime}
            runDB $ insert_ file
            return $ Just file
        Just _ -> return Nothing
