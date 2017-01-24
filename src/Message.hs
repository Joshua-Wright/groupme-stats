{-# LANGUAGE DeriveGeneric #-}
module Message(Message(..),MessageId) where
import           Data.Aeson
import           Data.Text
import           Data.Text
import           GHC.Generics
import           Prelude      hiding (id)
import qualified User         as U

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