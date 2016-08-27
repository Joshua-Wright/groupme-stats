{-# LANGUAGE DeriveGeneric #-}
module Message(Message(..)) where
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Prelude      hiding (id)

data Message = Message {
      name         :: Text
    , id           :: Text
    , text         :: Maybe Text
    , group_id     :: Text
    , user_id      :: Text
    , favorited_by :: [Text]
    , created_at   :: Int
    , system       :: Bool
    } deriving (Generic, Show, Eq)
instance FromJSON Message
instance ToJSON   Message where
    toEncoding = genericToEncoding defaultOptions

instance Ord Message where
    (Message{id=id1}) `compare` (Message{id=id2}) = id1 `compare` id2