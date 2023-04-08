{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Lib
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
  someFunc

app :: IO ()
app = do
  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents
        let q = any ((== SDL.QuitEvent) . SDL.eventPayload) events
        unless q loop
  loop

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
