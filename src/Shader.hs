module Shader
    ( module Shader
    ) where

import Graphics.Rendering.OpenGL as GL
import Data.Functor ((<&>))
import System.FilePath
import Control.Exception
import Control.Monad.Except

data Shaders = Shaders
  { vert :: GL.Shader,
    frag :: GL.Shader
  } deriving (Show, Eq)

createShaders :: FilePath -> String -> ExceptT String IO Shaders
createShaders dir name = do
  vert <- getShaderSrc (dir </> name ++ ".vert") >>= compile GL.VertexShader >>= checkError
  frag <- getShaderSrc (dir </> name ++ ".frag") >>= compile GL.FragmentShader >>= checkError
  pure $ Shaders vert frag


compile :: GL.ShaderType -> String -> ExceptT String IO GL.Shader
compile sType src = do
    shader <- liftIO $ GL.createShader sType
    let srcVar = GL.shaderSourceBS shader
    srcVar $= GL.packUtf8 src
    liftIO $ GL.compileShader shader
    liftIO GL.releaseShaderCompiler
    pure shader

checkError :: GL.Shader -> ExceptT String IO GL.Shader
checkError shader = do
    let statusVar = GL.compileStatus shader
    isSuccess <- get statusVar
    if isSuccess
    then pure shader
    else do
        err <- liftIO $ GL.shaderInfoLog shader
        throwError $ "Shader compilation failed: " ++ err

getShaderSrc :: FilePath -> ExceptT String IO String
getShaderSrc path = do
    res <- liftIO $ catch (Right <$> readFile path) handleErr
    eitherToExcept res
    where
        handleErr :: IOException -> IO (Either String String)
        handleErr _ = pure $ Left ("Failed to load file: " ++ path)

eitherToExcept :: Either a b -> ExceptT a IO b
eitherToExcept (Right r) = pure r
eitherToExcept (Left e) = throwError e

composeShaders :: Either String a -> Either String a -> (a -> a -> b) -> Either String b
composeShaders vert frag shaders = (vert <&> shaders) <*> frag
