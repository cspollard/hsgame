{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Graphics.Gloss.Data.Bitmap

import Control.FRPNow.Core

makePrisms ''GG.Event

type GEvent = GG.Event


main :: IO ()
main = do let d = InWindow "hello" (1000, 1000) (0, 0)
          let c = black
          let fps = 60
          p <- loadBMP "data/test.bmp"

          play d c fps p id (flip const) (flip const)
