module Main where

import Data.Proxy (Proxy (..))
import Data.Typelevel.Exists (someTypeRep)
import Data.Typelevel.Passage
  ( Passage (recite),
    birote,
    embody,
    invoke,
    passage,
    rote,
    summon,
  )
import Data.Void (Void)
import Test.Hspec (describe, hspec, it, shouldBe)
import Type.Reflection (Typeable)

main :: IO ()
main = hspec $ do
  describe "grimoire" $ do
    it "allows you to summon a type" $ do
      let result = recite (summon @Typeable) (Proxy @Void) `invoke` embody
      result `shouldBe` someTypeRep @Void

    it "allows you to summon a structure of types" $ do
      let result = recite (rote @[] $ birote @(,) embody embody) (Proxy @['("foo", String), '("bar", Void)])
      result
        `shouldBe` [ (someTypeRep @"foo", someTypeRep @String),
                     (someTypeRep @"bar", someTypeRep @Void)
                   ]

    it "allows you to lift your own passages by rote" $ do
      let myPassage = passage @MyClass myOperation
          result = recite (rote $ birote (rote myPassage) myPassage) (Proxy @['( 'Just A, B), '( 'Nothing, C)])
      result
        `shouldBe` [ (Just "A", "B"),
                     (Nothing, "C")
                   ]

class MyClass a where
  myOperation :: Proxy a -> String

data A

data B

data C

instance MyClass A where
  myOperation _ = "A"

instance MyClass B where
  myOperation _ = "B"

instance MyClass C where
  myOperation _ = "C"
