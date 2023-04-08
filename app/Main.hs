{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import SDL

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "hs-graphics" windowConfig
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  app
  SDL.destroyWindow window
  SDL.quit

app :: IO ()
app = do
  events <- readEvents
  let q = any ((== SDL.QuitEvent) . SDL.eventPayload) events
  unless q app

readEvents :: IO [Event]
readEvents = do
  e <- SDL.pollEvent
  case e of
    Just e' -> (e' :) <$> readEvents
    _ -> return []

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
