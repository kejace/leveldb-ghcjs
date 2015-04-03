module Main (main) where

import           Test.Tasty
import 			 Database.LevelDB.Core
import           Database.LevelDB.Example

main :: IO ()
main = defaultMain $ testGroup "Tests" []
