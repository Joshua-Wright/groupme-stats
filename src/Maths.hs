{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Maths where
import           BasicPrelude          (tshow)
import           Prelude               hiding (show)
-- import qualified BasicPrelude as BP
import           Data.Function
import           Data.List
import qualified Data.Map.Strict       as Map
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import qualified Data.Set              as Set
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
import qualified Group                 as G
import qualified LocalData             as L
import qualified Message               as M
import qualified User                  as U
-- import Debug.Trace

data Like = Like {
      messageId       :: M.MessageId
    , userIdRecipient :: U.UserId
    , userIdLiked     :: U.UserId
    } deriving (Generic, Show, Eq, Ord)

likesForMessage :: M.Message -> [Like]
likesForMessage m = map (\uid -> defaultLike{userIdLiked=uid}) $ M.favorited_by m
    where
        defaultLike = Like {
              messageId       = M.id m
            , userIdRecipient = M.user_id m
            , userIdLiked     = ""
            }

allLikesList :: L.LocalData -> [Like]
allLikesList l = concatMap likesForMessage $ L.messages l

likesByUser :: U.UserId -> [Like] -> [Like]
likesByUser uid xs = filter (\x -> userIdLiked x == uid) xs

likesToUser :: U.UserId -> [Like] -> [Like]
likesToUser uid xs = filter (\x -> userIdRecipient x == uid) xs


-- zip to tuples of counts, use map to group them adding the counts
groupByFreq :: (Ord a) => [a] -> [(a, Int)]
groupByFreq xs = Map.toList $ Map.fromListWith (+) $ zip xs (repeat 1)

groupByFreqKey :: (Ord b) => (a -> b) -> [a] -> [(b, Int)]
groupByFreqKey mapper xs = groupByFreq $ map mapper xs

escape :: T.Text -> T.Text
escape str =T.concat ["\"", T.concatMap escapeChar str, "\""]
    where
        escapeChar :: Char -> T.Text
        escapeChar '\"' = "\\\""
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar '_'  = "\\_"
        escapeChar x    = T.pack [x]

numPairListToDat :: Show a => [(a,a)] -> T.Text
numPairListToDat xs = T.intercalate "\n" $ map toLine xs
    where toLine (a,b) = T.concat [tshow a, " ", tshow b]

assocListToDat :: Show a => [(T.Text, a)] -> T.Text
assocListToDat xs = T.intercalate "\n" $ map toLine $ zip [1..] xs
    where
        toLine (idx, (label, content)) = T.concat [tshow idx, " ", escape label, " ", tshow content]

gridToDat :: (Show b) => [((T.Text,T.Text), b)] -> T.Text
gridToDat gridMap = T.unlines $ (header : (map row keys ))
    where
        keys = Set.toList $ Set.fromList $ ( map fst $ map fst gridMap ) ++ ( map snd $ map fst gridMap )
        header = T.unwords $ "\"\"" : (map (escape) keys)
        row k1 = T.unwords $ (escape k1) : [ lookup (k1, k2) | k2 <- keys ]
        shownMap = Map.map tshow $ Map.fromList gridMap
        lookup x = Map.findWithDefault "0" x shownMap

--------


likesReceivedByUserData :: L.LocalData -> [(U.UserId, Int)]
likesReceivedByUserData l = groupByFreqKey likeKey (allLikesList l)
    where likeKey x = L.userIdToName l $ userIdRecipient x

likesReceivedByUser :: L.LocalData -> T.Text
likesReceivedByUser = assocListToDat . likesReceivedByUserData


likesGivenByUserData :: L.LocalData -> [(U.UserId, Int)]
likesGivenByUserData l = groupByFreqKey likeKey (allLikesList l)
    where likeKey x = L.userIdToName l $ userIdLiked x

-- list of user IDs to .dat of their frequency
likesGivenByUser :: L.LocalData -> T.Text
likesGivenByUser = assocListToDat . likesGivenByUserData



likesGivenByUserToUserData :: L.LocalData -> [((U.UserId, U.UserId), Int)]
likesGivenByUserToUserData l = groupByFreqKey likeKey (allLikesList l)
    where likeKey Like{userIdLiked=a,userIdRecipient=b} = (L.userIdToName l a, L.userIdToName l b)

allLikesGivenByUserToUser :: L.LocalData -> T.Text
allLikesGivenByUserToUser = gridToDat . likesGivenByUserToUserData



---------------------------------------------------------

allRawText :: L.LocalData -> T.Text
allRawText l = T.unlines $ catMaybes $ map M.text $ L.messages l

rawTextByUser :: L.LocalData -> T.Text -> T.Text
rawTextByUser l userName = T.unlines $ catMaybes $ map M.text userMsgs
    where
        msgs = L.messages l
        Just user = find ((== userName) . U.nickname) $ G.members $ L.group l
        userId = U.user_id user
        userMsgs = filter ((== userId) . M.user_id) msgs

wordFrequency :: L.LocalData -> T.Text
wordFrequency l = T.unlines $ map lineToDat $ sortBy (comparing $ Down . snd) wordFrequency
    where
        rawText = allRawText l
        wordFrequency = groupByFreq $ T.words rawText
        lineToDat (a,b) = T.unwords [tshow b, a]

allTimes :: L.LocalData -> T.Text
allTimes l = T.intercalate "\n" times
    where
        msgs = L.messages l
        times = map (tshow . posixSecondsToUTCTime . fromIntegral . M.created_at) msgs

likesFromUserToUser :: [M.Message] -> T.Text -> T.Text -> Int
likesFromUserToUser msgs fromId toId = length likedMsgs
    where
        userMsgs = filter (\m -> (M.user_id m) == toId) msgs
        likedMsgs = filter (\m -> fromId `elem` (M.favorited_by m)) userMsgs

usagePerTime :: L.LocalData -> T.Text
usagePerTime l = T.unlines $ map toDatLine $ zip [0..] $ usagePerTimeData $ L.messages l
    where
        toDatLine (a,b) = T.unwords [tshow a, tshow b ]

usagePerTimePerUser :: L.LocalData -> T.Text
usagePerTimePerUser l = T.unlines $ (header : (map toDatLine $ transpose allUserData))
    where
        userIds = map U.user_id $ G.members $ L.group l
        header :: T.Text
        header = T.unwords $ "\"\"" : (map (escape . userName) userIds)
        usagePerUser :: T.Text -> [Int]
        usagePerUser userId = usagePerTimeData $ filter (\m -> (M.user_id m) == userId) msgs
        allUserData :: [[Int]]
        allUserData = [0..23] : map usagePerUser userIds
        toDatLine :: [Int] -> T.Text
        toDatLine xs = T.unwords $ map tshow xs
        --
        msgs = L.messages l
        userName = L.userIdToName l


-- gets number of messages sent per each hour of the day
usagePerTimeData :: [M.Message] -> [Int]
usagePerTimeData xs = map getHour [0..23]
    where
        -- utcTime behaves like a Num of seconds
        hourOfMessage :: M.Message -> Int
        hourOfMessage m = ((`div` 3600) . floor . utctDayTime . posixSecondsToUTCTime . fromIntegral . M.created_at) m
        -- get hours of all the messages
        msgHours = map hourOfMessage xs
        msgsByHour = Map.fromListWith (+) $ zip  msgHours (repeat 1)
        getHour hour = Map.findWithDefault 0 hour msgsByHour

allMessageLengths :: L.LocalData -> T.Text
allMessageLengths l = T.unlines $ map tshow $ lengths
    where
        -- not actually *all* message lengths, just the 90th percentile
        msgs = L.messages l
        nMsgs = floor $ (9%10) * (fromIntegral . length $ msgs)
        lengths :: [Int]
        lengths = take nMsgs $ sort $ map T.length $ catMaybes $ map M.text msgs


messageLengthsByUser :: L.LocalData -> T.Text
messageLengthsByUser l = "" {-todo-}
    where
        nBoxes = 10
        dx = nMsgs `div` nBoxes
        nMsgs = floor $ (9%10) * (fromIntegral . length $ msgs)
        lengthBounds = [(x, x+dx-1) | x <- [0,dx..nBoxes] ]
        msgCounts min max msgs = map (messagesOfLength min max) msgs
        msgs = L.messages l



messagesOfLength :: Int -> Int -> [M.Message] -> [M.Message]
messagesOfLength minLen maxLen msgs = filter pred msgs
    where pred m = all ($ m) [ ((>= minLen) . T.length . (fromMaybe "") . M.text)
                             , ((<= maxLen) . T.length . (fromMaybe "") . M.text)
                             ]
messagesByUser :: T.Text -> [M.Message] -> [M.Message]
messagesByUser userId msgs = filter pred msgs
    where pred = ((== userId) . M.user_id)
messagesOfLengthByUser :: T.Text -> Int -> Int -> [M.Message] -> Int
messagesOfLengthByUser userId minLen maxLen msgs =
    length $ messagesOfLength minLen maxLen $ messagesByUser userId msgs

