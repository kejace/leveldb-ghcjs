{-# LANGUAGE OverloadedStrings, JavaScriptFFI, ForeignFunctionInterface, InterruptibleFFI #-}

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
    , testCC
    , testDD
--    , testEE
    )

where

import Control.Monad.IO.Class

import           Database.LevelDB.Types
import           Data.ByteString    (ByteString)
import           Data.ByteString.Char8 as BC
import           Data.Default
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import qualified Data.Text as T

import Control.Concurrent
import Control.Concurrent.MVar

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

--instance MonadIO IO where
--    liftIO = IO

--backends = ['locket', 'memdown', 'level-js', 'levelup']  --, { db: require('level-js') } --, { db: require('locket') }
-- db.open({ createIfMissing: true })"
foreign import javascript unsafe "$r = levelup($1, { db: require('memdown')});"
    openFFI :: JSString -> IO (JSRef ())
    
    --"db = h$leveljs; db.put($1, $2, function (err, value) { $r += value; $r += err}); $r='put'; "
foreign import javascript interruptible "$1.put($2, $3, function(err, value) { $c('put ' + value + ' ' + err); });" 
    putFFI :: JSRef a -> JSString -> JSString -> IO JSString

foreign import javascript interruptible "$1.get($2 , function (err, value) { $c('' +value); });" 
    getFFI :: JSRef a -> JSString -> IO JSString

foreign import javascript unsafe "var levelup = require('level-js'), h$leveljs = levelup('/tmp/testdb_levelup',  { h$leveljs: require('locket') }); db.open({ createIfMissing: true }); $r=h$levelup;" 
    gopenFFI :: Int -> IO (JSRef a)
--var levelup = require('levelup'), db = levelup('/tmp/testdb_levelup');
foreign import javascript unsafe "db = h$leveljs; db.put($1, $2, function (err, value) { $r += value; $r += err}); $r='put'; "  
    gputFFI :: JSString -> JSString -> IO JSString
--var levelup = require('levelup'), db = levelup('/tmp/testdb_levelup');
foreign import javascript unsafe "db = h$leveljs; $r='hellllllllllo'; db.get($1 , function (err, value) { $r += value; $r += err});  $r+='get';" 
    ggetFFI :: JSString -> IO JSString


foreign import javascript unsafe "$1();" runCallback :: JSFun (IO [JSString]) -> IO ()

open :: MonadIO m => FilePath -> Options -> m DB
--open :: FilePath -> Options -> DB
open p o = do
    --mv1 <- newEmptyMVar
    test1 <- liftIO $ openFFI $ toJSString p
--    test2 <- liftIO $ putFFI test1 (toJSString $ "hello") (toJSString $ "world")
--    test3 <- liftIO $ getFFI test1 (toJSString $ "hello")
   -- s1 <- syncCallback AlwaysRetain True (liftIO $ openFFI 2 >> takeMVar mv1 >> return "error")
  --  runCallback s1
   -- threadDelay 1000000
    --putMVar mv1 ()
    --Prelude.putStr $ fromJSString test

    --str <- T.unpack . fromJSString =<< openFFI 10
    return $ DB test1 o

put :: MonadIO m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
--put :: DB -> WriteOptions -> ByteString -> ByteString -> ()
put (DB db o) wo key val = do
        test <- liftIO $ putFFI db (toJSString $ BC.unpack key) (toJSString $ BC.unpack val)
        return ()
--put db wo key val = put' (toJSString . key) (toJSString. val)

--foreign import javascript unsafe "var levelup = require('levelup'), db = levelup('/does/not/matter', { db: require('memdown') }); db.put('name', 'Yuri Irsenovich Kim'); $r=''; db.get('name' , function (err, value) { $r += 'hw'; });  $r+='helllllo';"  --  db.readStream().on('data', console.log); $r = 'hello';
--  put' :: JSString -> JSString -> (IO JSString)

get :: MonadIO m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get (DB db o) ro k = do
    value <- liftIO $ getFFI db (toJSString $ BC.unpack k)
    return $ Just $ BC.pack $ fromJSString value -- fromJSString test

--get :: DB -> ReadOptions -> ByteString -> (Maybe ByteString)
--get db ro k = return $ Just $ BC.pack "I'm a value"

testCC :: IO ()
testCC =  do

    test1 <- liftIO $ openFFI "/tmp/testdb_levelup"
    test2 <- liftIO $ putFFI test1 "hello" "world"
    test3 <- liftIO $ getFFI test1 "hello"
    Prelude.putStrLn $ fromJSString test3

testDD :: IO ()
testDD =  do

    test1 <- liftIO $ gopenFFI 2
    gputFFI "hello" "world"
    test3 <- ggetFFI "hello"
    Prelude.putStrLn $ fromJSString test3

 -- testEE :: IO ()
 -- testEE = do ()
 --    mv1 <- newEmptyMVar
 --    mv2 <- newEmptyMVar
 --    mv3 <- newEmptyMVar
 --    c1 <- syncCallback AlwaysRetain True (gopenFFI 2 >> takeMVar mv1 >> putStrLn "hello_c1")
 --    c2 <- syncCallback AlwaysRetain True (gputFFI (toJSString $ "hello") (toJSString $ "world") >> takeMVar mv2 >> putStrLn "hello_c2")
 --    c3 <- syncCallback AlwaysRetain True (ggetFFI (toJSString $ "hello") >> takeMVar mv3 >> putStrLn "hello_c3")

 --    runCallback s1
 --    runCallback s2
 --    runCallback s3
