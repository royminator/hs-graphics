{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import SDL
import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "hs-graphics" windowConfig
    SDL.showWindow window
    SDL.glCreateContext window
    appLoop
    SDL.destroyWindow window
    SDL.quit
    putStrLn "hi"

appLoop :: IO ()
appLoop = do
  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        unless quit (loop)
  loop

windowConfig :: SDL.WindowConfig
windowConfig =
    SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL }
    
