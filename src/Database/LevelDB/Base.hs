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
    , open
    , put
    , get
    )

where

import           Database.LevelDB.Types
import           Data.ByteString    (ByteString)
import           Data.ByteString.Char8 as BC
import           Data.Default
import           Control.Monad.IO.Class   (MonadIO (liftIO))

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

--instance MonadIO IO where
--    liftIO = IO    

open :: MonadIO m => FilePath -> Options -> m DB
--open :: FilePath -> Options -> DB
open p o = return $ DB o

put :: MonadIO m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
--put :: DB -> WriteOptions -> ByteString -> ByteString -> ()
put db wo key val = return ()
--put db wo key val = put' (toJSString . key) (toJSString. val)

--foreign import javascript unsafe "var levelup = require('levelup'), db = levelup('/does/not/matter', { db: require('memdown') }); db.put('name', 'Yuri Irsenovich Kim'); $r=''; db.get('name' , function (err, value) { $r += 'hw'; });  $r+='helllllo';"  --  db.readStream().on('data', console.log); $r = 'hello';
--  put' :: JSString -> JSString -> (IO JSString)

get :: MonadIO m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
--get :: DB -> ReadOptions -> ByteString -> (Maybe ByteString)
get db ro k = return $ Just $ BC.pack "value"
