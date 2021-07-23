{-# LANGUAGE LambdaCase #-}

-- | This top-level module will be used by the acceptor application.
--   Acceptor application asks 'TraceObject's from the forwarder application.
module Trace.Forward.Acceptor
  ( runTraceAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Data.Typeable (Typeable)
import           System.Time.Extra (sleep)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Network.Acceptor (listenToForwarder)
import           Trace.Forward.Configuration (AcceptorConfiguration (..))
import           Trace.Forward.Protocol.Type (NodeInfo)

runTraceAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor configuration.
  -> ([lo] -> IO ())          -- ^ The handler for 'TraceObject's received from the node.
  -> (NodeInfo -> IO ())      -- ^ The handler for node's info received from the node.
  -> IO ()
runTraceAcceptor config loHandler niHandler =
  tryJust excludeAsyncExceptions (listenToForwarder config loHandler niHandler) >>= \case
    Left e -> do
      putStrLn $ "trace-forward, acceptor has a problem: " <> show e
      sleep 1.0
      runTraceAcceptor config loHandler niHandler
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e
