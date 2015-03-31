module Main (main) where

import           Test.Tasty
import 			 Database.LevelDBJS.Core

main :: IO ()
main = defaultMain $ testGroup "Tests" []
