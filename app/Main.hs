{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)

import Control.Lens

import SDL hiding (Event)
import qualified SDL.Event as SE

import Linear.Affine
import Linear.V2

import Control.FRPNow.Core

type SEvent = SE.Event
makeLenses ''EventPayload

main :: IO ()
main = do initializeAll
          window <- createWindow "My SDL Application" defaultWindow
          renderer <- createRenderer window (-1) defaultRenderer
          bkg <- createTextureFromSurface renderer =<< loadBMP "data/bkg.bmp"
          face <- createTextureFromSurface renderer =<< loadBMP "data/face.bmp"
          playFace bkg face renderer


playFace :: Texture -> Texture -> Renderer -> IO ()
playFace bkg face r = do e <- eventPayload <$> waitEvent
                         case e of
                              QuitEvent -> return ()
                              MouseMotionEvent mm -> do copy r bkg Nothing Nothing
                                                        copy r face Nothing . Just $ Rectangle (fromIntegral <$> mouseMotionEventPos mm) (V2 100 100)
                                                        present r
                                                        playFace bkg face r
                              _                   -> playFace bkg face r
