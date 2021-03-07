{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import AesonVia (AesonNewtype (..), AesonRecord (..), AesonTag (..), HasTagPrefix (..))
import Control.Newtype.Generics (Newtype)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype Author = Author
  { _authorWrapped :: String
  } deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonNewtype Author String)

instance Newtype Author

data Category =
    CategoryNews
  | CategoryOpinion
  | CategoryLies
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag Category)

instance HasTagPrefix Category where
  getTagPrefix _ = "Category"

data Article = Article
  { _articleTitle :: String
  , _articleAuthor :: Author
  , _articleCategory :: Category
  } deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord Article)

testSomething :: TestTree
testSomething = testCase "something" $ do
  let article = Article "Throw the ball!" (Author "Dog") CategoryOpinion
      expected = "{\"title\":\"Throw the ball!\",\"author\":\"Dog\",\"category\":\"opinion\"}"
      actual = TLE.decodeUtf8 (encode article)
  actual @?= expected

main :: IO ()
main = defaultMain $ testGroup "AesonVia"
  [ testSomething
  ]
