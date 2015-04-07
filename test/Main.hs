module Main (main) where

import           Test.Tasty
import 			 Database.LevelDB.Base
--import           Database.LevelDB.Example

main :: IO ()
main = defaultMain $ testGroup "Tests" []
