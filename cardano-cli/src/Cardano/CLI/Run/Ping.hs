{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Run.Ping
  ( PingClientCmdError (..)
  , renderPingClientCmdError
  , runPingCmd
  )
where

import           Cardano.CLI.Commands.Ping
import           Cardano.CLI.Pretty
import qualified Cardano.Network.Ping as CNP

import           Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import           Control.Exception (SomeException)
import           Control.Monad (forM, unless)
import           Control.Monad.Class.MonadAsync (MonadAsync (async, wait, waitCatch))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer (Tracer (..))
import           Data.List (foldl')
import qualified Data.List as L
import qualified Data.List as List
import           Network.Socket (AddrInfo)
import qualified Network.Socket as Socket
import qualified System.Exit as IO
import qualified System.IO as IO

newtype PingClientCmdError = PingClientCmdError [(AddrInfo, SomeException)]

maybeHostEndPoint :: EndPoint -> Maybe String
maybeHostEndPoint = \case
  HostEndPoint host -> Just host
  UnixSockEndPoint _ -> Nothing

maybeUnixSockEndPoint :: EndPoint -> Maybe String
maybeUnixSockEndPoint = \case
  HostEndPoint _ -> Nothing
  UnixSockEndPoint sock -> Just sock

pingClient
  :: Tracer IO CNP.LogMsg -> Tracer IO String -> PingCmd -> [CNP.NodeVersion] -> AddrInfo -> IO ()
pingClient stdout stderr cmd = CNP.pingClient stdout stderr opts
 where
  opts =
    CNP.PingOpts
      { CNP.pingOptsQuiet = pingCmdQuiet cmd
      , CNP.pingOptsJson = pingCmdJson cmd
      , CNP.pingOptsCount = pingCmdCount cmd
      , CNP.pingOptsHost = maybeHostEndPoint (pingCmdEndPoint cmd)
      , CNP.pingOptsUnixSock = maybeUnixSockEndPoint (pingCmdEndPoint cmd)
      , CNP.pingOptsPort = pingCmdPort cmd
      , CNP.pingOptsMagic = pingCmdMagic cmd
      , CNP.pingOptsHandshakeQuery = pingOptsHandshakeQuery cmd
      }

runPingCmd :: PingCmd -> ExceptT PingClientCmdError IO ()
runPingCmd options = do
  let hints = Socket.defaultHints{Socket.addrSocketType = Socket.Stream}

  msgQueue <- liftIO STM.newEmptyTMVarIO

  -- 'addresses' are all the endpoints to connect to and 'versions' are the node protocol versions
  -- to ping with.
  (addresses, versions) <- case pingCmdEndPoint options of
    HostEndPoint host -> do
      addrs <- liftIO $ Socket.getAddrInfo (Just hints) (Just host) (Just (pingCmdPort options))
      return (addrs, CNP.supportedNodeToNodeVersions $ pingCmdMagic options)
    UnixSockEndPoint fname -> do
      let addr =
            Socket.AddrInfo
              []
              Socket.AF_UNIX
              Socket.Stream
              Socket.defaultProtocol
              (Socket.SockAddrUnix fname)
              Nothing
      return ([addr], CNP.supportedNodeToClientVersions $ pingCmdMagic options)

  -- Logger async thread handle
  laid <- liftIO . async $ CNP.logger msgQueue (pingCmdJson options) (pingOptsHandshakeQuery options)
  -- Ping client thread handles
  caids <-
    forM addresses $
      liftIO . async . pingClient (Tracer $ doLog msgQueue) (Tracer doErrLog) options versions
  res <- L.zip addresses <$> mapM (liftIO . waitCatch) caids
  liftIO $ doLog msgQueue CNP.LogEnd
  liftIO $ wait laid

  -- Collect errors 'es' from failed pings and 'addrs' from successful pings.
  let (es, addrs) = foldl' partition ([], []) res

  -- Report any errors
  case (es, addrs) of
    ([], _) -> liftIO IO.exitSuccess
    (_, []) -> left $ PingClientCmdError es
    (_, _) -> do
      unless (pingCmdQuiet options) $ mapM_ (liftIO . IO.hPrint IO.stderr) es
      liftIO IO.exitSuccess
 where
  partition
    :: ([(AddrInfo, SomeException)], [AddrInfo])
    -> (AddrInfo, Either SomeException ())
    -> ([(AddrInfo, SomeException)], [AddrInfo])
  partition (es, as) (a, Left e) = ((a, e) : es, as)
  partition (es, as) (a, Right _) = (es, a : as)

  doLog :: StrictTMVar IO CNP.LogMsg -> CNP.LogMsg -> IO ()
  doLog msgQueue msg = STM.atomically $ STM.putTMVar msgQueue msg

  doErrLog :: String -> IO ()
  doErrLog = IO.hPutStrLn IO.stderr

renderPingClientCmdError :: PingClientCmdError -> Doc ann
renderPingClientCmdError = \case
  PingClientCmdError es -> mconcat $ List.intersperse "\n" $ pshow <$> es
