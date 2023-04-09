{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (pack)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.Rendering.OpenGL as GL
import Graphics.VinylGL
import Linear
import SDL
import System.FilePath ((</>))

type Pos = "position" ::: V3 GLfloat

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (pack "hs-graphics") windowConfig
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  app window
  SDL.destroyWindow window
  SDL.quit

app :: SDL.Window -> IO ()
app window = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  events <- readEvents
  let q = any ((== SDL.QuitEvent) . SDL.eventPayload) events
  d <- dunno
  d
  SDL.glSwapWindow window
  unless q (app window)

dunno :: IO (IO ())
dunno = do
  shader <- simpleShaderProgram ("src" </> "shaders" </> "triangle2d.vert") ("src" </> "shaders" </> "triangle2d.frag")
  verts <- bufferVertices triangle
  vao <- makeVAO $ do
    enableVertices' shader verts
    bindVertices verts
  return . withVAO vao $
    do currentProgram $= Just (program shader)

readEvents :: IO [Event]
readEvents = do
  e <- SDL.pollEvent
  case e of
    Just e' -> (e' :) <$> readEvents
    _ -> return []

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}

triangle :: [FieldRec '[Pos]]
triangle =
  map
    pos
    [ V3 (-0.5) (-0.5) 0.0,
      V3 0.5 (-0.5) 0.0,
      V3 0.5 0.0 0.0
    ]

pos :: V3 GLfloat -> FieldRec '[Pos]
pos = (:& RNil) . Field
