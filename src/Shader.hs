module Shader
    ( module Shader
    ) where

import Graphics.Rendering.OpenGL as GL
import Data.Functor ((<&>))
import System.FilePath
import Control.Exception

data Shaders = Shaders
  { vert :: GL.Shader,
    frag :: GL.Shader
  } deriving (Show, Eq)

createShaders :: FilePath -> String -> IO (Either String Shaders)
createShaders dir name = do
  vertSrc <- getShaderSrc $ dir </> name ++ ".vert"
  fragSrc <- getShaderSrc $ dir </> name ++ ".frag"
  vert <- compile GL.VertexShader vertSrc
  frag <- compile GL.FragmentShader fragSrc
  pure $ composeShaders vert frag Shaders


compile :: GL.ShaderType -> Either String String -> IO (Either String GL.Shader)
compile _ (Left e) = pure $ Left e
compile sType (Right src) = do
    shader <- GL.createShader sType
    let srcVar = GL.shaderSourceBS shader
    srcVar $= GL.packUtf8 src
    GL.compileShader shader
    GL.releaseShaderCompiler
    checkError shader

checkError :: GL.Shader -> IO (Either String GL.Shader)
checkError shader = do
    let statusVar = GL.compileStatus shader
    isSuccess <- get statusVar
    if isSuccess
    then pure $ Right shader
    else do
        err <- GL.shaderInfoLog shader
        pure $ Left $ "Shader compilation failed: " ++ err

getShaderSrc :: FilePath -> IO (Either String String)
getShaderSrc path = 
    (Right <$> readFile path) `catch`
    ((\_ -> pure (Left ("Failed to load file: " ++ path))) :: IOException -> IO (Either String String))

composeShaders :: Either String a -> Either String a -> (a -> a -> b) -> Either String b
composeShaders vert frag shaders = (vert <&> shaders) <*> frag
