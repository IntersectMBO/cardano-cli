{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Ping.Run
  ( PingClientCmdError (..)
  , renderPingClientCmdError
  , runPingCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as CNP

import Control.Exception (SomeException, toException)
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync (mapConcurrently)
import Control.Tracer (mkTracer)
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.List qualified as L
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Network.Socket (AddrInfo)
import Network.Socket qualified as Socket
import System.IO qualified as IO
import Text.Printf (printf)

data PingClientCmdError
  = PingClientCmdError [(AddrInfo, SomeException)]
  | PingClientMisconfigurationError String
  deriving Show

instance Error PingClientCmdError where
  prettyError = renderPingClientCmdError

runPingCmd :: PingCmd -> CIO e ()
runPingCmd options
  | Just err <- getConfigurationError options =
      throwCliError $ PingClientMisconfigurationError err
runPingCmd options = do
  let hints = Socket.defaultHints{Socket.addrSocketType = Socket.Stream}

  addresses <- case pingCmdEndPoint options of
    HostEndPoint host ->
      liftIO $ Socket.getAddrInfo (Just hints) (Just host) (Just (pingCmdPort options))
    UnixSockEndPoint fname ->
      pure
        [ Socket.AddrInfo
            []
            Socket.AF_UNIX
            Socket.Stream
            Socket.defaultProtocol
            (Socket.SockAddrUnix fname)
            Nothing
        ]

  let stdout = mkTracer (TLIO.putStrLn . renderLogMsg (pingCmdJson options))
      stderr = mkTracer (IO.hPutStrLn IO.stderr . renderPingWarning)

  res <-
    liftIO $
      mapConcurrently
        (\addr -> (,) addr <$> CNP.pingClient stdout stderr (toPingOpts options) addr)
        addresses

  case L.foldl' partition ([], []) res of
    ([], _) -> pure ()
    (_, []) -> throwCliError $ PingClientCmdError es
    (_, _) -> unless (pingCmdQuiet options) $ mapM_ (liftIO . IO.hPrint IO.stderr) es
 where
  partition
    :: ([(AddrInfo, SomeException)], [AddrInfo])
    -> (AddrInfo, Either CNP.PingClientException ())
    -> ([(AddrInfo, SomeException)], [AddrInfo])
  partition (es, as) (a, Left e) = ((a, toException e) : es, as)
  partition (es, as) (a, Right _) = (es, a : as)

toPingOpts :: PingCmd -> CNP.PingOpts
toPingOpts cmd =
  CNP.PingOpts
    { CNP.pingOptsCount = pingCmdCount cmd
    , CNP.pingOptsMagic = CNP.NetworkMagic (pingCmdMagic cmd)
    , CNP.pingOptsJson = if pingCmdJson cmd then CNP.AsJSON else CNP.AsText
    , CNP.pingOptsQuiet = pingCmdQuiet cmd
    , CNP.pingOptsMode =
        if pingOptsGetTip cmd
          then CNP.TipMode
          else
            if pingOptsHandshakeQuery cmd
              then CNP.QueryMode
              else CNP.PingMode
    , -- cardano-cli has no flags for these yet, so use network's own defaults.
      CNP.pingOptsSRVPrefix = "_cardano._tcp"
    , CNP.pingOptsColor = CNP.ColorAuto
    }

-- | Format a ping log message. Mirrors the network library's internal
-- @format@/@ToText@ helpers, which are not exported.
renderLogMsg :: Bool -> CNP.WithHost CNP.LogMsg -> TL.Text
renderLogMsg True msg = encodeToLazyText (Aeson.toJSON msg)
renderLogMsg False (CNP.WithHost host logMsg) =
  TL.pack (printf "%-47s" (show host <> ", ")) <> renderLogMsgText logMsg

renderLogMsgText :: CNP.LogMsg -> TL.Text
renderLogMsgText = \case
  CNP.LogChainSyncTip tip -> TL.pack (show tip)
  CNP.LogStatPoint point -> TL.pack (show point)
  CNP.LogNodeToClientVersionData version versionData ->
    TL.pack (unwords [show version, either T.unpack show versionData])
  CNP.LogNodeToNodeVersionData version versionData ->
    TL.pack (unwords [show version, either T.unpack show versionData])

-- | Format a ping warning. Mirrors the network library's internal
-- @formatPingWarning@, which is not exported.
renderPingWarning :: CNP.PingWarning -> String
renderPingWarning = \case
  CNP.FilePathDoesNotExist path -> "WARNING: file path " <> show path <> " does not exist"
  CNP.DNSError domain err -> "WARNING: dns: " <> show domain <> " " <> show err
  CNP.DNSResolution domain ips port ->
    show domain <> ": " <> List.intercalate ", " [show ip <> ":" <> show port | ip <- ips]
  CNP.MissingPort ip -> "WARNING: missing port for " <> show ip
  CNP.Error err -> "WARNING: " <> show err
  CNP.ConnectError sockAddr err -> "WARNING: " <> show sockAddr <> " " <> show err

renderPingClientCmdError :: PingClientCmdError -> Doc ann
renderPingClientCmdError = \case
  PingClientCmdError es -> mconcat $ List.intersperse "\n" $ pshow <$> es
  PingClientMisconfigurationError err -> pretty err
