{-# LANGUAGE OverloadedStrings #-}
module Handler.Vote
    ( getVoteInfoR
    , postVoteActR
    , postVoteAddR
    ) where

import           Database.Persist         ((==.), (=.), (+=.))
import           Database.Persist.Types   (Filter, Entity(..), entityKey, updateValue)
import           Database.Persist.Class   (selectFirst, selectList, insert, update)
import           Database.Persist.MongoDB (nestEq, (->.), push, nestInc, eachOp)
import           Data.Maybe               (catMaybes)
import qualified Data.Text as T
import           Data.Text                (Text)
import           Foundation
import           Model
import           Text.Blaze.Html          (Html)
import           Yesod.Persist.Core       (runDB)
import           Yesod.Form.Input         (FormInput, iopt, ireq)
import           Yesod.Form.Fields        (intField, textField)
import           Yesod.Form.Input         (runInputGet, runInputPost)
import           Yesod.Core.Handler       (invalidArgs)

data VoteReq = VoteReq
    { reqVoteId   :: Maybe Int
    , reqChoiceId :: Maybe Int
    } deriving (Show)

data VoteAct = VoteAct
    { chip        :: Text
    , actVoteId   :: Int
    , actChoiceId :: Int
    } deriving (Show)

-- TODO: If this gets a UI integration a AForm would be useful
{-| Parse input on voteInfoR into a VoteReq -}
voteReqIForm :: FormInput Handler VoteReq
voteReqIForm = VoteReq -- TODO: Negative ID check
    <$> iopt intField "id"
    <*> iopt intField "choice"

voteActIForm :: FormInput Handler VoteAct
voteActIForm = VoteAct
    <$> ireq textField "chip"
    <*> ireq intField "id"
    <*> ireq intField "choice"

{-| Handle GET on /api/vote/info -}
-- TODO: Show as JSON
getVoteInfoR :: Handler Text
getVoteInfoR = do
    inp <- runInputGet voteReqIForm
    rec <- runDB $ selectList (reqToDBFilter inp) []
    return $ T.pack $ show rec

{-| Create a list of Mongo filters based on a VoteReq -}
reqToDBFilter :: VoteReq -> [Filter Vote]
reqToDBFilter req = catMaybes go
    where go = [(VoteIdentity ==.) <$> reqVoteId req,
                (nestEq $ VoteChoices ->. ChoiceIdentity) <$> reqChoiceId req] -- TODO: Obsolete?

{-| Handle POST on /api/vote/vote -}
postVoteActR :: Handler ()
postVoteActR = do
    inp <- runInputPost voteActIForm
    user <- runDB $ selectFirst [UserChipId ==. chip inp] []
    vote <- runDB $ selectFirst [VoteIdentity ==. actVoteId inp,
                                 VoteChoices ->. ChoiceIdentity `nestEq` actChoiceId inp] []
    case (user, vote) of
        (Just u, Just v) -> if isAllowed u v
                                then updateVote u v inp
                                else invalidArgs ["You already voted"]
        _                -> invalidArgs ["User or Vote not found"]

{-| Vote. Update all neccessary records -}
updateVote :: Entity User -> Entity Vote -> VoteAct -> Handler ()
updateVote user vote inp = do
    let choices = voteChoices . entityVal $ vote
    runDB $ update (entityKey vote) [push VoteVoted (entityKey user)]
    runDB $ update (entityKey vote) [VoteChoices =. updateChoices inp choices]

updateChoices :: VoteAct -> [Choice] -> [Choice]
updateChoices VoteAct{actChoiceId = cid} = map inc
    where inc x = if choiceIdentity x == cid
                      then x { choiceVotes = choiceVotes x + 1}
                      else x

{-| Check wether user is allowed to vote -}
isAllowed :: Entity User -> Entity Vote -> Bool
isAllowed user vote = not $ elem (entityKey user) (voteVoted . entityVal $ vote)

postVoteAddR :: Handler Text
postVoteAddR = undefined
