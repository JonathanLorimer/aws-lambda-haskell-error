module Formatters where

import Data.Bson                
import Data.Int                 (Int64)
import Data.Text
import SQLInterface             (CodeData(..))
import Data.Time                (NominalDiffTime(..))
import Data.Time.Clock          (UTCTime)
import Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Database.Persist         (Entity(..))
import Database.MongoDB         (Document, (=:), Value(..))

parsePosixToStamp :: NominalDiffTime -> Value
parsePosixToStamp = Stamp . MongoStamp . fromIntegral . floor . toRational

parseStampToUTC :: Value -> UTCTime
parseStampToUTC (Stamp (MongoStamp value)) = posixSecondsToUTCTime . realToFrac $ value

parseCodesToDocument :: [Entity CodeData] -> [Document]
parseCodesToDocument = Prelude.foldr f []
    where
        f (Entity _ codeData) list = [ "code"           =: codeDataCode codeData
                                     , "codeId"         =: codeDataCodeId codeData
                                     , "key"            =: codeDataKey codeData
                                     , "dateCreated"    =: (parsePosixToStamp . utcTimeToPOSIXSeconds $ codeDataDateCreated codeData)
                                     ] : list

