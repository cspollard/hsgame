{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Foreign.C.Types (CInt)

import Control.Monad.IO.Class

import Control.Lens

import SDL

import Linear.Affine
import Linear.V2

import Conduit

makeLenses ''EventPayload

textureSize :: MonadIO m => Texture -> m (V2 CInt)
textureSize t = do ti <- queryTexture t
                   let w = textureWidth ti
                   let h = textureHeight ti
                   return $ V2 w h


events :: MonadIO m => Source m Event
events = do yield =<< waitEvent
            events


main :: IO ()
main = do initializeAll
          window <- createWindow "My SDL Application" defaultWindow
          renderer <- createRenderer window (-1) defaultRenderer
          bkg <- createTextureFromSurface renderer =<< loadBMP "data/bkg.bmp"
          face <- createTextureFromSurface renderer =<< loadBMP "data/face.bmp"
          ts <- textureSize bkg
          windowSize window SDL.$= ts

          events $$ playFace bkg face renderer


playFace :: Texture -> Texture -> Renderer -> Sink Event IO ()
playFace bkg face rend = do copy rend bkg Nothing Nothing
                            copy rend face Nothing . Just $ Rectangle origin (V2 100 100)
                            present rend

                            go bkg face rend


    where go b f r = do e <- await
                        liftIO $ print e
                        case e of
                             Nothing -> go b f r
                             Just evt -> case eventPayload evt of
                                              QuitEvent -> return ()
                                              MouseMotionEvent mm -> do copy r bkg Nothing Nothing
                                                                        copy r face Nothing . Just $ Rectangle (fromIntegral <$> mouseMotionEventPos mm) (V2 100 100)
                                                                        present r
                                                                        go b f r
                                              _ -> go b f r
