{-# LANGUAGE ExtendedDefaultRules       #-}

module MongoInterface where

import Data.Text
import qualified Data.Map as M
import Database.MongoDB
import Database.MongoDB.Connection

insertCodes :: [Document] -> Action IO [Value]
insertCodes = insertMany "code-lookup"

findGreatestDate :: Action IO (Maybe Document)
findGreatestDate = findOne (select [] "code-lookup") {sort = ["dateCreated" =: -1], limit = 1}

mkHost :: M.Map String String -> Maybe Host
mkHost env = do
    host <- M.lookup "MONGO_HOST" env
    port <- M.lookup "MONGO_PORT" env
    return $ Host host $ portNumber port
        where
            portNumber :: String -> PortID
            portNumber p = PortNumber . fromInteger . read $ p

data Credentials = Credentials { user :: Text
                               , password :: Text
                               } deriving (Show)

getCredsFromEnv :: M.Map String String -> Maybe Credentials
getCredsFromEnv env = do
    user        <- pack <$> M.lookup "MONGO_USER" env
    password    <- pack <$> M.lookup "MONGO_PASSWORD" env
    return $ Credentials user password
