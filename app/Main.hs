{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Debug.Trace

import Foreign.C.Types (CInt)

import Data.Word (Word32)

import Control.Concurrent.STM
import Data.Conduit.TQueue
import Data.Conduit.TMChan

import Control.Monad (unless)
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


data Redraw = Redraw deriving Show
type MyEvent = Either Redraw Event


timer :: MonadIO m => Word32 -> Source m Redraw
timer fps = do q <- liftIO . atomically $ newTBQueue 1
               addTimer fps $ \_ -> do atomically $ writeTBQueue q Redraw
                                       return (Reschedule fps)
               sourceTBQueue q


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

          evts <- runResourceT $ mapOutput Right events >=< mapOutput Left (timer 500)
          evts $$ playFace bkg face renderer


playFace :: Texture -> Texture -> Renderer -> Sink MyEvent IO ()
playFace bkg face r = do e <- await
                         liftIO $ print e
                         case fmap eventPayload <$> e of
                              Nothing -> return ()
                              Just (Right QuitEvent) -> return ()
                              Just e' -> do go e'; playFace bkg face r
                            

    where go evt = case evt of
                        Right (MouseMotionEvent mm) -> do copy r bkg Nothing Nothing
                                                          copy r face Nothing . Just $ Rectangle (fromIntegral <$> mouseMotionEventPos mm) (V2 100 100)
                                                          -- present r
                        Left Redraw -> present r
                        _ -> return ()
