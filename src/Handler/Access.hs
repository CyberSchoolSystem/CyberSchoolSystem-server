{-# LANGUAGE OverloadedStrings #-}
module Handler.Access 
    ( postApiAccessInR
    , postApiAccessOutR
    , postApiAccessExportR
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Bits                (xor)
import qualified Data.ByteString.Lazy     as BL
import           Data.Default             (def)
import qualified Data.HashMap.Lazy        as HML
import           Data.List                (sortBy)
import           Data.Monoid              ((<>))
import           Data.Time.LocalTime      (ZonedTime(..), TimeZone(), zonedTimeToUTC, utcToZonedTime)
import qualified Data.Text                as T
import           Data.Time.Calendar       (Day(), fromGregorian)
import           Data.Time.Clock          (UTCTime(..), getCurrentTime, diffUTCTime, addUTCTime)
import           Data.Time.Format         (FormatTime(..), formatTime, defaultTimeLocale)
import           Database.Persist         ((==.))
import           Database.Persist.Class   (selectFirst, selectList, update)
import           Database.Persist.Types   (Entity(..))
import           Database.Persist.MongoDB (push)
import qualified Error                    as E
import           Foundation
import           Handler.File             (addFile)
import qualified Message                  as M
import           Model
import           System.Time.Utils        (renderSecs)
import           Text.Pandoc.Builder      (Pandoc, Blocks, Alignment(..),
                                           doc, table, text, plain, bulletList)
import           Text.Pandoc.Writers.Docx (writeDocx)
import           Text.Pandoc              (setUserDataDir, runIO)
import           Yesod.Auth               (requireAuthId)
import           Yesod.Core.Json          (requireJsonBody)
import           Yesod.Persist.Core       (runDB)

type Gradename = T.Text
data AccessUser = AccessUser -- TODO: Newtype?
    { idUser :: T.Text
    }

data AccessExport = AccessExport
    { exportDay    :: ZonedTime
    }

instance FromJSON AccessUser where
    parseJSON (Object v) = AccessUser 
        <$> v .: "username"
    parseJSON invalid = typeMismatch "AccessUser" invalid

instance FromJSON AccessExport where
    parseJSON (Object v) = AccessExport
        <$> v .: "day"
    parseJSON invalid = typeMismatch "AccessUser" invalid

{-| Access -}
postApiAccessInR :: Handler Value
postApiAccessInR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserUsername ==. idUser req] []
    addToDB True req user

{-| Check wether use is inside -}
isInside :: User -> Bool
isInside u = accessInside . maximum $ dummy : userAccess u
    where dummy = Access (UTCTime (fromGregorian 1970 1 1) 0) False

{-| Create a new Access with the current time -}
newAccess :: Bool -> IO Access
newAccess dir = Access <$> getCurrentTime <*> return dir

{-| Add a new Access to the db if the user changes state (i.e from inside to outside -}
addToDB :: Bool -> AccessUser -> Maybe (Entity User) -> Handler Value
addToDB direction req user =
    case user of
        Just u -> if direction `xor` (isInside . entityVal $ u)
                      then do
                          access <- liftIO $ newAccess direction
                          runDB $ update (entityKey u) [push UserAccess access]
                          return . toJSON $ (E.ENull :: E.Error Value)
                      else return . toJSON $ E.AlreadyDone (M.fromMessage $ M.AlreadyDone M.Border)
                                                        [("username", toJSON $ idUser req)
                                                        ,("inside", toJSON $ direction)]
        Nothing -> return . toJSON $ E.Unknown
                                         (M.fromMessage $ M.Unknown M.Username)
                                         [("username", idUser req)]

{-| Leave -}
postApiAccessOutR :: Handler Value
postApiAccessOutR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserUsername ==. idUser req] []
    addToDB False req user

{-| Render a pdf -}
postApiAccessExportR :: Handler Value
postApiAccessExportR = do
    req <- requireJsonBody :: Handler AccessExport
    auth <- requireAuthId
    now <- liftIO $ getCurrentTime
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just u -> do
            let day = utctDay . zonedTimeToUTC . exportDay $ req
                zone = zonedTimeZone . exportDay $ req
                filename = "export - "
                    <> (userUsername . entityVal $ u)
                    <> " "
                    <> (T.pack . show $ day) <> ".docx"
            (users, gradeN) <- loadGradeUsers (entityVal u)
            add <- addFile (entityKey u)
                "application/vnd.openxmlformats-officedocument.wordprocessingml"
                (addUTCTime 20 now) -- Delete in 5 minutes
                filename
            saveFile add (renderGradeTable day zone users gradeN)
        Nothing -> return . toJSON $
            E.Unknown (M.fromMessage $ M.Unknown M.User) [("username", user)]

{-| Save a file if it was saved into the db -}
saveFile :: Maybe File -> Pandoc -> Handler Value
saveFile add pan = do
    case add of
        Nothing ->
            return . toJSON $
                E.AlreadyDone (M.fromMessage $ M.AlreadyDone M.Export)
                    ([] :: [(T.Text,T.Text)])
        Just file -> do
            liftIO $ savePDF pan (fileFilePath file)
            return . mergeAeson $ [(toJSON (E.ENull :: E.Error Value)), (toJSON file)]

{-| Load all users and the grade a teacher is responsible for -}
loadGradeUsers :: User -> Handler ([User], Gradename)
loadGradeUsers User{userRoles = r} = do
    let grade = roleTeacher r
    case grade of
        Just g -> do
            u <- fmap entityVal <$> (runDB $ selectList [UserGrade ==. g] [])
            name <- runDB $ selectFirst [GradeId ==. g] []
            case name of
                Just n -> return (u, gradeName . entityVal $ n)
                Nothing -> error "Unknown grade. This should not happen."
        Nothing -> error "Teacher role already checked by login system. This should not happen"

{-| Render information -}
renderGradeTable :: Day -> TimeZone -> [User] -> Gradename -> Pandoc
renderGradeTable day zone users gradeN =
    doc $ table (text . T.unpack $ gradeN)
                [(AlignLeft, 1), (AlignLeft, 1), (AlignLeft, 1), (AlignLeft, 1)]
                (tblocks <$> ["Name", "Anwesend von ... bis ...", "Zeit anwesend (in h)", "Kommentar"])
                (userToRow <$> sorted)
    where sorted = sortBy lastName users
          lastName x y = compare (userLastName x) (userLastName y)
          userToRow u = let timestamp = timeInside day zone u in
              [plain . text . T.unpack $ (userLastName u) <> " " <> (userFirstName u),
              fst timestamp,
              snd timestamp,
              tblocks ""]

{-| Get the time User was present (numerically and as intervals) -}
timeInside :: Day -> TimeZone -> User -> (Blocks, Blocks)
timeInside day zone user = (interval, numeric)
    where
        interval = bulletList $ tupToMsg <$> tup
        numeric = tblocks $ renderSecs . ceiling . sum $ diffTup <$> tup
        tup = filter inDay $ accessTuples user
        inDay a = -- True if Both tuple elements are equal to Just day or Nothing
            (utctDay . fst $ a) == day && (maybe True (\x -> day == utctDay x) $ snd a)
        diffTup (s, em) =
            case em of
                Just e -> diffUTCTime e s / (60 * 60) -- In Hours
                Nothing -> 0
        tupToMsg (s, em) =
            case em of 
                Just e -> tblocks $
                    (showTime . utcToZonedTime zone $ s)
                    <> " bis "
                    <> (showTime . utcToZonedTime zone $ e)
                Nothing -> tblocks $ (showTime s) <>
                    " bis [nicht richtig abgemeldet, oder noch anwesend]"

{-| Load a users access list into sorted, logical tuples -}
accessTuples :: User -> [(UTCTime, Maybe UTCTime)]
accessTuples u = chunk $ accessTime <$> access -- TODO sort
    where
        access         = userAccess u
        chunk []       = []
        chunk [x]      = [(x, Nothing)]
        chunk (x:y:zs) = (x, Just y) : chunk zs

{-| Save a File and register it into global context -}
savePDF :: Pandoc -> FilePath -> IO ()
savePDF pandoc filepath = do
    pdf <- runIO $ do
        setUserDataDir Nothing
        writeDocx def pandoc
    case pdf of
        Left _ -> error "fail"
        Right p -> BL.writeFile filepath p

showTime :: FormatTime a => a -> String
showTime = formatTime defaultTimeLocale "%H:%M"

tblocks :: String -> Blocks
tblocks = plain . text

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map mrg
    where
        mrg (Object x) = x
        mrg _          = error "This should not happen"
