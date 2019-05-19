import Test.Hspec

type Decimal = Int
type ShadokNumber = String

shadok :: Decimal -> ShadokNumber
shadok 0  = "GA"
shadok 1  = "BU"
shadok 2  = "ZO"
shadok 3  = "MEU"
shadok x  = shadok ( x `div` 4 ) ++ shadok ( x `mod` 4 )


main = hspec $ do

    describe "Shadok numeric base" $ do
        it "0, 1, 2, 3 should be GA BU ZO MEU" $ do
             shadok 0 `shouldBe` "GA"
             shadok 1 `shouldBe` "BU"
             shadok 2 `shouldBe` "ZO"
             shadok 3 `shouldBe` "MEU"


    describe "Multiple of four" $ do
        it "With only one GA" $ do
            shadok 4  `shouldBe` "BUGA"
            shadok 8  `shouldBe` "ZOGA"
            shadok 12 `shouldBe` "MEUGA"

        it "With multiple GA" $ do
            shadok 16 `shouldBe` "BUGAGA"

    describe "Any number which is not a multiple of four" $ do
        it "two shadoks verb number" $ do
            shadok 7  `shouldBe` "BUMEU"
            shadok 15 `shouldBe` "MEUMEU"
            shadok 5  `shouldBe` "BUBU"

        it "three shadoks verb number" $ do
            shadok 19 `shouldBe` "BUGAMEU"

        it "big number" $ do
            shadok 9999 `shouldBe` "ZOBUMEUGAGAMEUMEU"

        it "very big number" $ do 
            shadok 47850 `shouldBe` "ZOMEUZOZOMEUZOZOZO"
