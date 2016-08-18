{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.Types (CInt)

import Data.Word (Word32)

import Control.Monad (unless)
import Control.Monad.IO.Class

import Control.Lens

import SDL hiding (Event)
import qualified SDL.Event as SE

import Linear.Affine
import Linear.V2

import Control.FRPNow

type SEvent = SE.Event
makeLenses ''EventPayload

textureSize :: MonadIO m => Texture -> m (V2 CInt)
textureSize t = do ti <- queryTexture t
                   let w = textureWidth ti
                   let h = textureHeight ti
                   return $ V2 w h


data Redraw = Redraw
type MyEvent = Either Redraw SEvent


setFPS :: Word32 -> Now (EvStream Redraw)
setFPS t = do (es, cb) <- callbackStream
              addTimer t $ \_ -> do cb Redraw; return (Reschedule t)
              return es


sevents :: Now (EvStream SEvent)
sevents = do (es, cb) <- callbackStream
             sync $ mapEvents cb
             return es


main :: IO ()
main = do initializeAll
          window <- createWindow "My SDL Application" defaultWindow
          renderer <- createRenderer window (-1) defaultRenderer
          bkg <- createTextureFromSurface renderer =<< loadBMP "data/bkg.bmp"
          face <- createTextureFromSurface renderer =<< loadBMP "data/face.bmp"
          ts <- textureSize bkg
          windowSize window $= ts
          runNowMaster $ do tstream <- fmap Left <$> setFPS 30
                            sevts <- fmap Right <$> sevents
                            callIOStream (playFace bkg face renderer) (merge tstream sevts)
                            async $ return ()


playFace :: Texture -> Texture -> Renderer -> MyEvent -> IO ()
playFace bkg face r e = case fmap eventPayload e of
                             Right QuitEvent -> return ()
                             Right (MouseMotionEvent mm) -> do copy r bkg Nothing Nothing
                                                               copy r face Nothing . Just $ Rectangle (fromIntegral <$> mouseMotionEventPos mm) (V2 100 100)
                             Left Redraw -> present r
                             _ -> return ()
