{-# LANGUAGE OverloadedStrings #-}
module Handler.Vote
    ( postApiVoteInfoR
    , postApiVoteActR
    , postApiVoteAddR
    , postApiVoteRemoveR
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Database.Persist         ((==.), (=.))
import           Database.Persist.Types   (Filter, Entity(..), entityKey)
import           Database.Persist.Class   (selectFirst, selectList, insert, update, delete)
import           Database.Persist.MongoDB (nestEq, (->.), push)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import qualified Data.HashMap.Lazy        as HML
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.LocalTime      (ZonedTime, zonedTimeToUTC)
import qualified Error                    as E
import           Foundation
import qualified Message                  as M
import           Model
import           Text.HTML.SanitizeXSS    (sanitizeBalance)
import           Yesod.Auth               (requireAuthId, maybeAuthId)
import           Yesod.Core.Json          (FromJSON, requireJsonBody)
import           Yesod.Persist.Core       (runDB)

{-| Differenciate between running and terminated votes -}
newtype VoteTerminated = VoteTerminated { voteTerminated :: Entity Vote }
newtype VoteRunning = VoteRunning { voteRunning :: Entity Vote }
newtype ChoiceTerminated = ChoiceTerminated { choiceTerminated :: Choice }
newtype ChoiceRunning = ChoiceRunning { choiceRunning :: Choice }

data VoteReq = VoteReq
    { reqVoteId   :: Maybe (Key Vote)
    , reqChoiceId :: Maybe Int
    } deriving (Show)

data ApiVoteAct = ApiVoteAct
    { actVoteId   :: Key Vote
    , actChoiceId :: Int
    } deriving (Show)

data ApiVoteAdd = ApiVoteAdd
    { addDesc     :: Text
    , addTitle    :: Text
    , addChoices  :: [Text]
    , addEOL      :: ZonedTime
    }

data VoteDel = VoteDel { delId :: Key Vote }

instance FromJSON VoteReq where
    parseJSON (Object v) = VoteReq
        <$> v .:? "vid"
        <*> v .:? "choice"
    parseJSON invalid = typeMismatch "VoteReq" invalid

instance FromJSON ApiVoteAct where
    parseJSON (Object v) = ApiVoteAct
        <$> v .: "vid"
        <*> v .: "choice"
    parseJSON invalid = typeMismatch "ApiVoteAct" invalid

instance FromJSON ApiVoteAdd where
    parseJSON (Object v) = ApiVoteAdd
        <$> v .: "description"
        <*> v .: "title"
        <*> v .: "choices"
        <*> v .: "endOfLife"
    parseJSON invalid = typeMismatch "ApiVoteAdd" invalid

instance FromJSON VoteDel where
    parseJSON (Object v) = VoteDel
        <$> v .: "vid"
    parseJSON invalid = typeMismatch "VoteDel" invalid

instance ToJSON VoteTerminated where
    toJSON VoteTerminated{voteTerminated = Entity{entityKey = k, entityVal = u}} = object
        [ "description" .= (sanitizeBalance $ voteDescription u)
        , "title" .= (sanitizeBalance $ voteTitle u)
        , "id" .= k
        , "choices" .= (ChoiceTerminated <$> voteChoices u)
        , "voted" .= (length $ voteVoted u)
        , "terminated" .= True
        , "endOfLife" .= voteEndOfLife u
        ]

instance ToJSON VoteRunning where
    toJSON VoteRunning{voteRunning = Entity{entityKey = k, entityVal = u}} = object
        [ "description" .= (sanitizeBalance $ voteDescription u)
        , "title" .= (sanitizeBalance $ voteTitle u)
        , "id" .= k
        , "choices" .= (ChoiceRunning <$> voteChoices u)
        , "terminated" .= False
        , "endOfLife" .= voteEndOfLife u
        ]

instance ToJSON ChoiceTerminated where
    toJSON ChoiceTerminated{choiceTerminated = u} = object
        [ "description" .= (sanitizeBalance $ choiceDescription u)
        , "votes" .= choiceVotes u
        , "identity" .= choiceIdentity u
        ]

instance ToJSON ChoiceRunning where
    toJSON ChoiceRunning{choiceRunning = u} = object
        [ "description" .= (sanitizeBalance $ choiceDescription u) 
        , "identity" .= choiceIdentity u
        ]

{-| Handle Post on /api/vote/info -}
postApiVoteInfoR :: Handler Value
postApiVoteInfoR = do
    maid <- maybeAuthId
    inp  <- requireJsonBody :: Handler VoteReq -- TODO: Check Application Type?
    rec <- runDB $ selectList (reqToDBFilter inp) []
    votes <- liftIO . sequence $ (toValue <$> rec)
    let voted = maidToVoted maid . entityVal <$> rec
    return . toJSON $ zipWith (\v b -> mergeAeson [v, object ["alreadyVoted" .= b]]) votes voted

{-| Check whether user has already voted -}
maidToVoted :: Maybe UserId -> Vote -> Bool
maidToVoted Nothing _ = False
maidToVoted (Just u) vote = hasVoted u vote
    where hasVoted i Vote{voteVoted = v} = elem i v

{-| Merge two json values -}
mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map go
    where go (Object x) = x
          go _ = undefined

{-| Create a list of Mongo filters based on a VoteReq -}
reqToDBFilter :: VoteReq -> [Filter Vote]
reqToDBFilter req = catMaybes go
    where go = [(VoteId ==.) <$> reqVoteId req,
                (nestEq $ VoteChoices ->. ChoiceIdentity) <$> reqChoiceId req] -- TODO: Obsolete?

{-| Create the correct JSON value of a vote -}
toValue :: Entity Vote -> IO Value
toValue v = do
    a <- active
    if a
        then return . toJSON . VoteRunning $ v
        else return . toJSON . VoteTerminated $ v
    where
        active = do
            t <- getCurrentTime
            return $ t < (voteEndOfLife $ entityVal v)

{-| Let users vote -}
postApiVoteActR :: Handler Value
postApiVoteActR = do
    inp <- requireJsonBody :: Handler ApiVoteAct
    auth <- requireAuthId
    vote <- runDB $ selectFirst [VoteId ==. actVoteId inp,
                                 VoteChoices ->. ChoiceIdentity `nestEq` actChoiceId inp] []
    user <- runDB $ selectFirst [UserId ==. auth] []
    case (user, vote) of
        (Just u, Just v) -> do
            a <- liftIO $ isActive (entityVal v)
            if a
                then
                    if isAllowed u v
                        then updateVote u v inp
                        else return . toJSON $ E.AlreadyDone
                                                   (M.fromMessage $ M.AlreadyDone M.Vote)
                                                   [("vid", actVoteId inp)]
                else return . toJSON $ E.TimedOut 
                                           (M.fromMessage $ M.TimedOut M.Vote)
                                           [("vid", actVoteId inp)]
        (_,_) -> return . toJSON . E.Unknown (M.fromMessage $ M.Unknown M.Vote) $
            catMaybes [ const ("userId", toJSON $ auth) <$> user
                      , const ("vid", toJSON $ actVoteId inp) <$> vote]

{-| Vote. Update all neccessary records -}
updateVote :: Entity User -> Entity Vote -> ApiVoteAct -> Handler Value
updateVote user vote inp = do
    let choices = voteChoices . entityVal $ vote
    runDB $ update (entityKey vote) [push VoteVoted (entityKey user)]
    runDB $ update (entityKey vote) [VoteChoices =. updateChoices inp choices]
    return . toJSON $ (E.ENull :: E.Error Value)

{-| Insert Description -}
updateChoices :: ApiVoteAct -> [Choice] -> [Choice]
updateChoices ApiVoteAct{actChoiceId = cid} = map inc
    where inc x = if choiceIdentity x == cid
                      then x { choiceVotes = choiceVotes x + 1}
                      else x

{-| Check wether user is allowed to vote -}
isAllowed :: Entity User -> Entity Vote -> Bool
isAllowed user vote = not $ elem (entityKey user) (voteVoted . entityVal $ vote)

{-| Check whether the vote's eol is reached -}
isActive :: Vote -> IO Bool
isActive Vote{voteEndOfLife = v} = do
    t <- getCurrentTime
    if t > v
        then return False
        else return True

{-| Add a vote to the database -}
postApiVoteAddR :: Handler Value
postApiVoteAddR = do
    inp <- requireJsonBody :: Handler ApiVoteAdd
    _ <- runDB $ insert (addToVote inp)
    return $ toJSON (E.ENull :: E.Error Value)

{-| Transform ApiVoteAdd request to database entity -}
addToVote :: ApiVoteAdd -> Vote
addToVote v = Vote {
    voteDescription = addDesc v,
    voteTitle = addTitle v,
    voteVoted = [],
    voteEndOfLife = zonedTimeToUTC $ addEOL v,
    voteChoices = addToChoices v }

{-| Transform ApiVoteAdd request to Choice entity -}
addToChoices :: ApiVoteAdd -> [Choice]
addToChoices ApiVoteAdd{addChoices = c} = addInfo c 0
    where addInfo (t:ts) i = Choice i 0 t : addInfo ts (i+1)
          addInfo [] _ = []

{-| Remove a vote from the database -}
postApiVoteRemoveR :: Handler Value
postApiVoteRemoveR = do
    inp <- requireJsonBody :: Handler VoteDel
    result <- runDB $ selectFirst [VoteId ==. delId inp] []
    case result of
        Just ent -> runDB $ delete (entityKey ent) >> (return . toJSON $ (E.ENull :: E.Error Value))
        Nothing  -> return . toJSON $ E.Unknown "This vote is unknown" [("vid", delId inp)]
