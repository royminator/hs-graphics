{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (pack)
import Graphics.Rendering.OpenGL as GL
import SDL
import Shader

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
  GL.clearColor $= Color4 0.8 0.2 0.2 1
  GL.clear [ColorBuffer]
  SDL.glSwapWindow window
  events <- readEvents
  let q = any ((== SDL.QuitEvent) . SDL.eventPayload) events
  unless q (app window)

triangleVerts :: [V2 GLfloat]
triangleVerts =
  [ V2 (-0.5) (-0.5),
    V2 0.5 (-0.5),
    V2 0.0 0.5
  ]

readEvents :: IO [Event]
readEvents = do
  e <- SDL.pollEvent
  case e of
    Just e' -> (e' :) <$> readEvents
    _ -> return []

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
