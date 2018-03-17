{-# LANGUAGE PartialTypeSignatures #-}

import Test.Hspec
import Control.Monad.Bread

main :: IO ()
main = hspec $ do
    describe "withCrumb" $ do
        it "is inside-out" $ do
            runBread (withCrumb 'a' (withCrumb 'b' crumbs))
                `shouldBe`
                    (Right "ba" :: Either (_ _ Int) String)
        it "collects with the error" $ do
            runBread (withCrumb 'a' (withCrumb 'b' (exit "no")))
                `shouldBe`
                    (Left ("ba", "no") :: Either _ Int)
