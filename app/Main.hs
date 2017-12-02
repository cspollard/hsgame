{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2
import           Pipes
import           Pipes.Concurrent
import           SDL                    hiding (get)


textureSize :: MonadIO m => Texture -> m (V2 CInt)
textureSize t = do
  ti <- queryTexture t
  let w = textureWidth ti
  let h = textureHeight ti
  return $ V2 w h


main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  bkg <- createTextureFromSurface renderer =<< loadBMP "data/bkg.bmp"
  face <- createTextureFromSurface renderer =<< loadBMP "data/face.bmp"
  ts <- textureSize bkg
  windowSize window $= ts

  (eventsOut, eventsIn) <- spawn $ bounded 100
  loc <- newTVarIO (Just origin)

  void . forkIO . runEffect
    $ fromInput eventsIn >-> hoist atomically (eventHandler loc)

  let loop = do
        mv2 <- readTVarIO loc
        case mv2 of
          Nothing -> return ()
          Just v2 -> do
            copy renderer bkg Nothing Nothing
            copy renderer face Nothing . Just $ Rectangle v2 (V2 100 100)
            present renderer
            es <- pollEvents
            runEffect $ each es >-> toOutput eventsOut
            loop

  loop

  where
    eventHandler loc = do
      e <- await
      case eventPayload e of
        MouseMotionEvent mm -> do
          lift . writeTVar loc . Just
            $ fromIntegral <$> mouseMotionEventPos mm
          eventHandler loc

        QuitEvent -> lift $ writeTVar loc Nothing

        _ -> eventHandler loc
