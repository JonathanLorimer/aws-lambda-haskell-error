{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

module SQLInterface where

import qualified    Data.Map                    as M
import              Data.Text
import              Data.Time.Clock.POSIX       (utcTimeToPOSIXSeconds)
import              Control.Monad.IO.Class
import              Control.Monad.Reader
import              Control.Monad.Logger        (runStdoutLoggingT, LoggingT)
import              Database.Persist
import              Database.Persist.MySQL 
import qualified    Database.Persist.TH         as PTH
import              Data.Time.Clock
import              GHC.Generics

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
    CodeData sql=code_lookup
        code Text
        codeId Text
        key Text
        dateCreated UTCTime
        UniqueKey key
        Primary codeId code
        deriving Show Read Generic
|]

mkSQLDBConnection :: M.Map String String -> Maybe ConnectInfo
mkSQLDBConnection env = do
    host        <- M.lookup "SQL_HOST" env
    port        <- M.lookup "SQL_PORT" env
    user        <- M.lookup "SQL_USER" env
    password    <- M.lookup "SQL_PASSWORD" env
    db          <- M.lookup "SQL_DB" env
    return ConnectInfo { connectHost        = host
                       , connectPort        = read port
                       , connectUser        = user
                       , connectPassword    = password
                       , connectDatabase    = db
                       , connectOptions     = []
                       , connectPath        = ""
                       , connectSSL         = Nothing
                       }

runAction :: ConnectInfo -> SqlPersistT (LoggingT IO) b ->  IO b
runAction connection action = runStdoutLoggingT 
                 $ withMySQLConn connection
                 $ \backend ->
                        runReaderT action backend

selectAfterDate :: (MonadIO m) => UTCTime -> SqlPersistT m [Entity CodeData]
selectAfterDate maxTime = selectList [CodeDataDateCreated >. maxTime ] []

getCodeAfterDate :: ConnectInfo -> UTCTime -> IO [Entity CodeData]
getCodeAfterDate connection = runAction connection . selectAfterDate

