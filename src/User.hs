{-# LANGUAGE DeriveGeneric #-}
module User(User(..)) where
import           Data.Text
import           Data.Aeson
import           GHC.Generics
import           Prelude      hiding (id)

data User = User {
      user_id   :: Text
    , id        :: Text
    , nickname  :: Text
    , image_url :: Maybe Text
    , muted     :: Bool
    } deriving (Generic, Show)
instance FromJSON User
instance ToJSON   User where
    toEncoding = genericToEncoding defaultOptions
