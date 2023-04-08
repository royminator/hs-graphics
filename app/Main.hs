{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Graphics.Rendering.OpenGL as GL
import SDL

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "hs-graphics" windowConfig
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
  SDL.glSwapWindow window
  unless q (app window)

readEvents :: IO [Event]
readEvents = do
  e <- SDL.pollEvent
  case e of
    Just e' -> (e' :) <$> readEvents
    _ -> return []

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
