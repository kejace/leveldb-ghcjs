module Database.LevelDB.Base 

    ( -- * Exported Types
      DB
    , Options (..)

    , ReadOptions (..)
    , WriteOptions (..)

    -- * Defaults
    , defaultOptions
    , defaultReadOptions
    , defaultWriteOptions

    -- * Basic Database Manipulations
    , baseOpen
    , basePut
    , baseGet
    )

where

import           Database.LevelDB.Types
import           Data.ByteString    (ByteString)
import           Data.ByteString.Char8 as BC
import           Data.Default
import           Control.Monad.IO.Class   (MonadIO (liftIO))

--instance MonadIO IO where
--    liftIO = IO    

--open :: MonadIO m => FilePath -> Options -> m DB
baseOpen :: FilePath -> Options -> DB
baseOpen p o = DB o

--put :: MonadIO m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
basePut :: DB -> WriteOptions -> ByteString -> ByteString -> ()
basePut db wo key val =  ()

--get :: MonadIO m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
baseGet :: DB -> ReadOptions -> ByteString -> (Maybe ByteString)
baseGet db ro k = Just $ BC.pack "value"
