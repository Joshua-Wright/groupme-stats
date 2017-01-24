{-# LANGUAGE DeriveGeneric #-}
module LocalData(
  LocalData(group,messages)
, userIdToName
, makeLocalData
) where
import           Data.Aeson
import qualified Data.Set        as S
import           Data.List hiding (group)
import           Data.Function
import           Data.Maybe
import           GHC.Generics
import qualified Group           as G
import qualified Message         as Msg
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Prelude       hiding (id)

data LocalData = LocalData {
      group    :: G.Group
    , messages :: [Msg.Message]
    , userIdNameMap :: Map.Map T.Text T.Text
    } deriving (Generic, Show)
instance FromJSON LocalData
instance ToJSON   LocalData where
    toEncoding = genericToEncoding defaultOptions

userIdToName :: LocalData -> T.Text -> T.Text
userIdToName l user_id = fromMaybe user_id
    $ Map.lookup user_id 
    $ userIdNameMap l


makeLocalData :: G.Group -> [Msg.Message] -> LocalData
makeLocalData grp msgs = LocalData {
      group = grp
    , messages = msgs
    , userIdNameMap = Map.map Msg.name $ lastMessageByUserId msgs
    }

lastMessageByUserId :: [Msg.Message] -> Map.Map T.Text Msg.Message
lastMessageByUserId msgs = 
      Map.unions
    $ reverse 
    $ map (\m -> Map.singleton (Msg.user_id m) m)
    $ sortBy (compare `on` Msg.created_at) msgs
