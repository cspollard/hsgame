{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Behavior                         hiding (lift, map, mapM)
import           Control.Category
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Graphics.Gloss.Interface.IO.Game
import           Pipes
import           Pipes.Concurrent
import           Pipes.Lift
import qualified Pipes.Prelude                    as P
import           Prelude                          hiding (id, (.))


type EventHandler m = BehaviorT m Event

eventHandler
  :: (MonadTrans t, MonadReader (TVar Picture) (t STM))
  => BehaviorT (t STM) Event ()
eventHandler = BehaviorT $ \e ->
      case e of
        EventKey (MouseButton LeftButton) Down _ pos
          -> return (leftMouseDown pos, ())
        _                                  -> return (eventHandler, ())

leftMouseDown
  :: (MonadTrans t, MonadReader (TVar Picture) (t STM))
  => (Float, Float) -> BehaviorT (t STM) Event ()
leftMouseDown pos = BehaviorT $ \e ->
  case e of
    EventKey (MouseButton LeftButton) Up _ _ -> do
      p <- ask
      lift $ writeTVar p Blank
      return (eventHandler, ())

    EventMotion pos' -> do
      p <- ask
      lift . modifyTVar p $ mappend . color red $ line [pos, pos']
      return (leftMouseDown pos', ())

    _ -> return (leftMouseDown pos, ())


main :: IO ()
main = do
  world <- newTVarIO Blank

  (eventsOut, eventsIn) <- spawn unbounded

  void . forkIO . runEffect . runReaderP world
    $ fromInput eventsIn
      >-> hoist (hoist atomically) (liftPipe eventHandler)
      >-> P.drain


  playIO
    (InWindow "Hello!" (800, 800) (800, 800))
    black
    10
    world
    readTVarIO
    (\e w -> atomically (send eventsOut e) >> return w)
    (\_ w -> return w)
