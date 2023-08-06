import Test.Hspec
import Shader

data DummyShaders = DummyShaders
    { vert :: Int
    , frag :: Int
    } deriving (Show, Eq)

main :: IO ()
main = hspec $ do
    describe "composeShaders" $ do
        it "with valid shaders, should be right" $
           shouldBe (composeShaders (Right 3) (Right 2) DummyShaders) (Right (DummyShaders 3 2))
        it "with invalid fragment shader, should be left" $
            shouldBe (composeShaders (Right 3) (Left "Failed") DummyShaders) (Left "Failed")
        it "with invalid vertex shader, should be left" $
            shouldBe (composeShaders (Left "Failed") (Right 3) DummyShaders) (Left "Failed")
        it "with both shaders invalid, should be left" $
            shouldBe (composeShaders (Left "Failed") (Left "hello") DummyShaders) (Left "Failed")
