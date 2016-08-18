{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.Types (CInt)

import Control.Monad (unless)
import Control.Monad.IO.Class

import Control.Lens

import SDL hiding (Event)
import qualified SDL.Event as SE

import Linear.Affine
import Linear.V2

import Control.FRPNow.Core

type SEvent = SE.Event
makeLenses ''EventPayload

textureSize :: MonadIO m => Texture -> m (V2 CInt)
textureSize t = do ti <- queryTexture t
                   let w = textureWidth ti
                   let h = textureHeight ti
                   return $ V2 w h

main :: IO ()
main = do initializeAll
          window <- createWindow "My SDL Application" defaultWindow
          renderer <- createRenderer window (-1) defaultRenderer
          bkg <- createTextureFromSurface renderer =<< loadBMP "data/bkg.bmp"
          face <- createTextureFromSurface renderer =<< loadBMP "data/face.bmp"
          ts <- textureSize bkg
          windowSize window $= ts
          runNowMaster (playFace bkg face renderer)


playFace :: Texture -> Texture -> Renderer -> Now (Event ())
playFace bkg face r = do e <- sync $ eventPayload <$> waitEvent
                         case e of
                              QuitEvent -> async $ return ()
                              MouseMotionEvent mm -> do copy r bkg Nothing Nothing
                                                        copy r face Nothing . Just $ Rectangle (fromIntegral <$> mouseMotionEventPos mm) (V2 100 100)
                                                        present r
                                                        playFace bkg face r
                              _                   -> playFace bkg face r
