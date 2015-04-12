module Database.LevelDB.Types
    ( 
      DB (..)
    , Options (..)
    , ReadOptions (..)
    , WriteOptions (..)

    , defaultOptions
    , defaultReadOptions
    , defaultWriteOptions
    )
where


import GHCJS.Foreign
import GHCJS.Types

import           Data.ByteString    (ByteString)
import           Data.ByteString.Char8 as BC
import           Data.Default
import           Control.Monad.IO.Class   (MonadIO (liftIO))


import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal


--newtype LevelDBJS a = (IO (JSRef a))

--data DB = DB LevelDBPtr Options (IORef Bool) -- Options'
data DB = DB (JSRef ()) Options

--newtype Snapshot = Snapshot SnapshotPtr deriving (Eq)

data Compression = NoCompression


newtype Comparator = Comparator (ByteString -> ByteString -> Ordering)

data FilterPolicy = FilterPolicy
    { fpName       :: String
    , createFilter :: [ByteString] -> ByteString
    , keyMayMatch  :: ByteString -> ByteString -> Bool
    }

--newtype BloomFilter = BloomFilter FilterPolicyPtr


data Options = Options
    { blockRestartInterval :: !Int
    , blockSize            :: !Int
    , cacheSize            :: !Int
    , comparator           :: !(Maybe Comparator)
    , compression          :: !Compression
    , createIfMissing      :: !Bool
    , errorIfExists        :: !Bool
    , maxOpenFiles         :: !Int
    , paranoidChecks       :: !Bool
 --   , writeBufferSize      :: !Int
 --   , filterPolicy         :: !(Maybe (Either BloomFilter FilterPolicy))
    }

defaultOptions :: Options
defaultOptions = Options
    { blockRestartInterval = 16
    , blockSize            = 4096
    , cacheSize            = 0
    , comparator           = Nothing
    , compression          = NoCompression
    , createIfMissing      = False
    , errorIfExists        = False
    , maxOpenFiles         = 1000
    , paranoidChecks       = False
 --   , writeBufferSize      = 4 `shift` 20
 --   , filterPolicy         = Nothing
    }

instance Default Options where
    def = defaultOptions

-- | Options for write operations
data WriteOptions = WriteOptions
    { sync :: !Bool
    } deriving (Eq, Show)

defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions { sync = False }

instance Default WriteOptions where
    def = defaultWriteOptions

-- | Options for read operations
data ReadOptions = ReadOptions
    { verifyCheckSums :: !Bool
    , fillCache       :: !Bool
 --   , useSnapshot     :: !(Maybe Snapshot)
    } deriving (Eq)

defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
    { verifyCheckSums = False
    , fillCache       = True
 --   , useSnapshot     = Nothing
    }

instance Default ReadOptions where
    def = defaultReadOptions


