import Test.Hspec
import Shader

main :: IO ()
main = hspec $ do
    describe "createShaders" $ do
        it "should be right" $
           shouldBe (createShaders (Right 3) (Right 2)) (Right (Shaders 3 2))
        it "should be left" $
            shouldBe (createShaders (Right 3) (Left "Failed")) (Left "Failed")
