{-# LANGUAGE DeriveGeneric #-}
module Message(
  Message(..)
, MessageId
, msgTime
, msgTimeText
) where
import           BasicPrelude          (tshow)
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Prelude               hiding (id, show)
import           Prelude               hiding (id)
import qualified User                  as U

type MessageId = Text
data Message = Message {
      name         :: Text
    , id           :: MessageId
    , text         :: Maybe Text
    , group_id     :: Text
    , user_id      :: U.UserId
    , favorited_by :: [Text]
    , created_at   :: Int
    , system       :: Bool
    } deriving (Generic, Show, Eq)
instance FromJSON Message
instance ToJSON   Message where
    toEncoding = genericToEncoding defaultOptions

instance Ord Message where
    (Message{id=id1}) `compare` (Message{id=id2}) = id1 `compare` id2

msgTime :: Message -> UTCTime
msgTime = posixSecondsToUTCTime . fromIntegral . created_at

msgTimeText :: Message -> Text
msgTimeText = tshow . posixSecondsToUTCTime . fromIntegral . created_at
