module Shader
    ( module Shader
    ) where

import Graphics.Rendering.OpenGL as GL
import System.FilePath

data Shaders = Shaders
  { vert :: GL.GLuint,
    frag :: GL.GLuint
  } deriving (Show, Eq)

loadShaders :: FilePath -> String -> IO (Either String Shaders)
loadShaders dir name = do
  vert <- loadShader (dir </> name </> ".vert") GL.VertexShader
  frag <- loadShader (dir </> name </> ".frag") GL.FragmentShader
  pure (Shaders <$> vert <*> frag)

loadShader :: FilePath -> GL.ShaderType -> IO (Either String GL.GLuint)
loadShader path shaderType = undefined

getShaderSrc :: FilePath -> IO String
getShaderSrc path = undefined

createShaders :: Either String GL.GLuint -> Either String GL.GLuint -> Either String Shaders
createShaders vert frag = (vert >>= pure . Shaders) <*> frag

