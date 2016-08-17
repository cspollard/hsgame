{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)

import Control.Lens

import SDL hiding (Event)
import qualified SDL.Event as SE

import Control.FRPNow.Core

type SEvent = SE.Event
makeLensesWith (lensRulesFor [("eventPayload", "_eventPayload")]) ''SE.Event

main :: IO ()
main = do initializeAll
          window <- createWindow "My SDL Application" defaultWindow
          renderer <- createRenderer window (-1) defaultRenderer
          t <- createTextureFromSurface renderer =<< loadBMP "data/test.bmp"
          appLoop t renderer


appLoop :: Texture -> Renderer -> IO ()
appLoop t r = do evts <- fmap eventPayload <$> pollEvents
                 mapM_ print evts
                 copy r t Nothing Nothing
                 present r
                 unless (elem QuitEvent evts) $ appLoop t r
