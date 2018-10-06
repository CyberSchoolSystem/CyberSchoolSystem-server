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
import           Data.Default             (def)
import qualified Data.HashMap.Lazy        as HML
import           Data.List                (sortBy)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime(..), getCurrentTime, diffUTCTime)
import           Database.Persist         ((==.))
import           Database.Persist.Class   (selectFirst, selectList, update)
import           Database.Persist.Types   (Entity(..))
import           Database.Persist.MongoDB (push)
import qualified Error                    as E
import           Foundation
import           Handler.File             (addFile)
import qualified Message                  as M
import           Model
import           Text.Pandoc.Builder      (Pandoc, Blocks, Alignment(..),
                                           doc, table, text, plain, bulletList)
import           Text.Pandoc.Writers.HTML (writeHtml5String)
import           Text.Pandoc              (setUserDataDir, runIO)
import           Yesod.Auth               (requireAuthId)
import           Yesod.Core.Json          (requireJsonBody)
import           Yesod.Persist.Core       (runDB)

type Gradename = T.Text
data AccessUser = AccessUser -- TODO: Newtype?
    { idUser :: T.Text
    }

instance FromJSON AccessUser where
    parseJSON (Object v) = AccessUser 
        <$> v .: "username"
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
    auth <- requireAuthId
    now <- liftIO $ getCurrentTime
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just u -> do
            (users, gradeN) <- loadGradeUsers (entityVal u)
            add <- addFile (entityKey u) "text/html" now "export.html"
            saveFile add (renderGradeTable users gradeN)
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
renderGradeTable :: [User] -> Gradename -> Pandoc
renderGradeTable users gradeN =
    doc $ table (text . T.unpack $ gradeN)
                [(AlignLeft, 1), (AlignLeft, 1), (AlignLeft, 1), (AlignLeft, 1)]
                (tblocks <$> ["Name", "Anwesend von ... bis ...", "Zeit anwesend (in h)", "Kommentar"])
                (userToRow <$> sorted)
    where sorted = sortBy lastName users
          lastName x y = compare (userLastName x) (userLastName y)
          userToRow u = let timestamp = timeInside u in
              [plain . text . T.unpack $ (userLastName u) <> (userFirstName u),
              fst timestamp,
              snd timestamp,
              tblocks "test"]

{-| Get the time User was present (numerically and as intervals) -}
timeInside :: User -> (Blocks, Blocks)
timeInside user = (interval, numeric)
    where
        interval = bulletList $ tupToMsg <$> tup
        numeric = tblocks $ show . sum $ diffTup  <$> tup
        tup = accessTuples user
        diffTup (s, em) =
            case em of
                Just e -> diffUTCTime e s
                Nothing -> 0
        tupToMsg (s, em) =
            case em of 
                Just e -> tblocks $ (show s) <> " bis " <> (show $ e)
                Nothing -> tblocks $ (show s) <>
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
        writeHtml5String def pandoc
    case pdf of
        Left _ -> error "fail"
        Right p -> T.writeFile filepath p

tblocks :: String -> Blocks
tblocks = plain . text

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map mrg
    where
        mrg (Object x) = x
        mrg _          = error "This should not happen"
