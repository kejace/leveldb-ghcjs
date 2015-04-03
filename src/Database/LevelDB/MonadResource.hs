module Database.LevelDB.MonadResource

   (     
      DB

    , Options(..)
    , ReadOptions(..)
    , WriteOptions(..)

    , open
    , put
    , get

    , MonadResource (..)

    ) where 

import           Data.ByteString    (ByteString)
import           Control.Monad.Trans.Resource
import           Database.LevelDB.Base        (  DB 
                                               , Options, ReadOptions, WriteOptions
                                               , defaultOptions, defaultReadOptions, defaultWriteOptions)

import Database.LevelDB.Base as Base

--open :: FilePath -> Options -> DB
open :: MonadResource m => FilePath -> Options -> m DB
open p o = return $ Base.baseOpen p o

put :: MonadResource m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
put db wo key val = return () --Base.basePut

-- | Read a value by key
get :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get db ro key = return $ Base.baseGet db ro key
