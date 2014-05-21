-----------------------------------------------------------------------------
--
-- Module      :  ProductSpec
-- Copyright   :
-- License     :  GPL Nothing
--
-- Maintainer  :  dinkar.ganti@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ProductSpec (
    spec
) where


-- I still dont understand how to use the spec for testing, because there is quite
-- a bit of setup needed.?
import SpecHelper
import qualified Company as Co

import Data.Set as S
spec :: Spec
spec = do
    describe "Product" $ do
        context "Simple text" $ do
            it "parses exactly as-is" $ do
                23 `shouldBe` 23



