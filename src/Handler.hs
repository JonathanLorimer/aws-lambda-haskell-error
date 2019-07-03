{-# LANGUAGE DeriveAnyClass #-}

module Handler where

import Data.Aeson
import GHC.Generics
import System.Environment       (getEnvironment)
import Data.Bson
import Data.Text
import Data.Map                 (fromList)
import SQLInterface             (getCodeAfterDate, mkSQLDBConnection)
import MongoInterface           (insertCodes, findGreatestDate, mkHost, getCredsFromEnv, Credentials(..))
import Formatters               (parseCodesToDocument, parseStampToUTC)
import Database.MongoDB
import Aws.Lambda

data Response = Response
  { statusCode:: Int
  , responseBody :: String
  } deriving (Generic, ToJSON)

handler :: String -> Context -> IO (Either Response Response)
handler _ context = do 
    env <- getEnvironment
    let envMap = fromList env
    case mkHost envMap of
        Nothing -> pure $ Left Response { statusCode = 500, responseBody = "Error locating db information in the environment" }
        Just sbHost -> do
            mongoConn <- connect sbHost
            case getCredsFromEnv envMap of 
                Nothing -> pure $ Left Response { statusCode = 500, responseBody = "Error locating credentials in environment" }
                Just creds -> do
                    authentication <- access mongoConn master "code-db" $ auth (user creds) (password creds)
                    if not authentication
                    then pure $ Left Response { statusCode = 404, responseBody = "Invalid credentials" }
                    else do
                        greatestDate <- access mongoConn master "code-db" $ findGreatestDate
                        case greatestDate of
                            Nothing -> pure $ Left Response { statusCode = 500, responseBody = "No entries in db" }
                            Just entry -> case Database.MongoDB.look "dateCreated" entry of
                                Nothing -> pure $ Left Response { statusCode = 500, responseBody = "Error finding dateCreated from latest entry" } 
                                Just dateCreated -> case mkSQLDBConnection envMap of
                                    Nothing -> pure $ Left Response { statusCode = 500, responseBody = "Error locating credentials in the environment" }
                                    Just connection -> do
                                        codes <- getCodeAfterDate connection . parseStampToUTC $ dateCreated
                                        e   <- access mongoConn master "code-db"
                                            $  insertCodes
                                            $  parseCodesToDocument codes
                                        close mongoConn
                                        let numberOfEntries = show . Prelude.length $ e
                                        pure $ Right Response { statusCode = 200, responseBody = numberOfEntries ++ " entries made"  }

