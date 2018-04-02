{-# LANGUAGE OverloadedStrings #-}
module Handler.Vote
    ( postVoteInfoR
    , postVoteActR
    , postVoteAddR
    , postVoteRemoveR
    ) where

import           Database.Persist         ((==.), (=.))
import           Database.Persist.Types   (Filter, Entity(..), entityKey)
import           Database.Persist.Class   (selectFirst, selectList, insert, update, delete)
import           Database.Persist.MongoDB (nestEq, (->.), push)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import           Error
import           Foundation
import           Model
import           Yesod.Persist.Core       (runDB)
import           Yesod.Core.Json          (FromJSON, requireJsonBody)

data VoteReq = VoteReq
    { reqVoteId   :: Maybe Int
    , reqChoiceId :: Maybe Int
    } deriving (Show)

data VoteAct = VoteAct
    { chip        :: Text
    , actVoteId   :: Int
    , actChoiceId :: Int
    } deriving (Show)

data VoteAdd = VoteAdd
    { addDesc     :: Text
    , addChoices  :: [Text]
    }

data VoteDel = VoteDel { delId :: Int }

instance FromJSON VoteReq where
    parseJSON (Object v) = VoteReq
        <$> v .:? "vid"
        <*> v .:? "choice"
    parseJSON invalid = typeMismatch "VoteReq" invalid

instance FromJSON VoteAct where
    parseJSON (Object v) = VoteAct
        <$> v .: "chip" -- TODO: Transform to uid field
        <*> v .: "vid"
        <*> v .: "choice"
    parseJSON invalid = typeMismatch "VoteAct" invalid

instance FromJSON VoteAdd where
    parseJSON (Object v) = VoteAdd
        <$> v .: "description"
        <*> v .: "choices"
    parseJSON invalid = typeMismatch "VoteAdd" invalid

instance FromJSON VoteDel where
    parseJSON (Object v) = VoteDel
        <$> v .: "vid"
    parseJSON invalid = typeMismatch "VoteDel" invalid

{-| Handle Post on /api/vote/info -}
postVoteInfoR :: Handler Value
postVoteInfoR = do
    inp  <- requireJsonBody :: Handler VoteReq -- TODO: Check Application Type?
    rec <- runDB $ selectList (reqToDBFilter inp) []
    return $ toJSON rec

{-| Create a list of Mongo filters based on a VoteReq -}
reqToDBFilter :: VoteReq -> [Filter Vote]
reqToDBFilter req = catMaybes go
    where go = [(VoteIdentity ==.) <$> reqVoteId req,
                (nestEq $ VoteChoices ->. ChoiceIdentity) <$> reqChoiceId req] -- TODO: Obsolete?

{-| Handle POST on /api/vote/vote -}
postVoteActR :: Handler Value
postVoteActR = do
    inp <- requireJsonBody :: Handler VoteAct
    user <- runDB $ selectFirst [UserChipId ==. chip inp] []
    vote <- runDB $ selectFirst [VoteIdentity ==. actVoteId inp,
                                VoteChoices ->. ChoiceIdentity `nestEq` actChoiceId inp] []
    case (user, vote) of
        (Just u, Just v) -> if isAllowed u v
                                then updateVote u v inp
                                else return . toJSON $ PermissionDenied ("You already voted" :: Text)
        (_,_) -> return . toJSON . WrongFieldValue $
                   catMaybes [ const ("chip", toJSON $ chip inp) <$> user
                             , const ("vid", toJSON $ actVoteId inp) <$> vote ]

{-| Vote. Update all neccessary records -}
updateVote :: Entity User -> Entity Vote -> VoteAct -> Handler Value
updateVote user vote inp = do
    let choices = voteChoices . entityVal $ vote
    runDB $ update (entityKey vote) [push VoteVoted (entityKey user)]
    runDB $ update (entityKey vote) [VoteChoices =. updateChoices inp choices]
    return Null

{-| Insert Description -}
updateChoices :: VoteAct -> [Choice] -> [Choice]
updateChoices VoteAct{actChoiceId = cid} = map inc
    where inc x = if choiceIdentity x == cid
                      then x { choiceVotes = choiceVotes x + 1}
                      else x

{-| Check wether user is allowed to vote -}
isAllowed :: Entity User -> Entity Vote -> Bool
isAllowed user vote = not $ elem (entityKey user) (voteVoted . entityVal $ vote)

{-| Add a vote to the database -}
postVoteAddR :: Handler Value
postVoteAddR = do
    minId <- minVoteId
    inp <- requireJsonBody :: Handler VoteAdd
    key <- runDB $ insert (addToVote inp minId)
    return . toJSON $ key

{-| Find the smallest available voteId -}
minVoteId :: Handler Int
minVoteId = do
    votes <- runDB $ selectList [] []
    return $ minimumFree $ map (voteIdentity . entityVal) votes
        where minimumFree x = head $ filter (not . flip elem x) [1..]

{-| Transform VoteAdd request to database entity -}
addToVote :: VoteAdd -> Int -> Vote
addToVote v i = Vote {
    voteIdentity = i,
    voteDescription = addDesc v,
    voteVoted = [],
    voteChoices = addToChoices v }

{-| Transform VoteAdd request to Choice entity -}
addToChoices :: VoteAdd -> [Choice]
addToChoices VoteAdd{addChoices = c} = addInfo c 0
    where addInfo (t:ts) i = Choice i 0 t : addInfo ts (i+1)
          addInfo [] _ = []

{-| Remove a vote from the database -}
postVoteRemoveR :: Handler Value
postVoteRemoveR = do
    inp <- requireJsonBody :: Handler VoteDel
    result <- runDB $ selectFirst [VoteIdentity ==. delId inp] []
    case result of
        Just ent -> runDB $ delete (entityKey ent) >>= (\_ -> return Null)
        Nothing  -> return . toJSON $ WrongFieldValue [("vid", delId inp)]
