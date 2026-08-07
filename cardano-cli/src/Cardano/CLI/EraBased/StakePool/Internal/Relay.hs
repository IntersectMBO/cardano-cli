{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.StakePool.Internal.Relay
  ( validateStakePoolRelays
  , stakePoolRelayToAddr
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Certificate (StakePoolRelay (..))

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.Network.Ping qualified as Ping

import Control.Monad
import Control.Tracer (nullTracer, (>$<))
import Data.ByteString.Char8 qualified as BSC
import Data.IP (IP (IPv4, IPv6))

-- | Check that every relay is reachable, by connecting to it with
-- 'Ping.pingClients''. Fails with the collected errors if any relay cannot be
-- reached. This requires network access to the relays.
validateStakePoolRelays :: NetworkId -> [StakePoolRelay] -> CIO e ()
validateStakePoolRelays network relays = do
  relayAddrs <- concat <$> mapM stakePoolRelayToAddr relays

  -- Skip the ping when there are no relays to check: 'Ping.pingClients'' builds a
  -- DNS resolver from /etc/resolv.conf before it looks at its address list, so it
  -- fails outright on hosts without one.
  unless (null relayAddrs) $ do
    let pingOpts =
          Ping.PingOpts
            { Ping.pingOptsCount = 1
            , Ping.pingOptsMagic = toNetworkMagic network
            , Ping.pingOptsJson = Ping.AsText
            , Ping.pingOptsQuiet = True
            , Ping.pingOptsSRVPrefix = "_cardano._tcp"
            , Ping.pingOptsColor = Ping.ColorNever
            , Ping.pingOptsMode = Ping.TipMode
            , Ping.pingOptsHashType = Ping.FullHash
            }

    pingErrs <- liftIO $ do
      stderr <- Ping.mkStdErrTracer
      headerTracer <- Ping.mkHeaderTracer pingOpts stderr
      Ping.pingClients'
        (Ping.format Ping.AsText >$< stderr)
        nullTracer
        headerTracer
        (Ping.toText >$< stderr)
        pingOpts
        Ping.AddressIsNotAFilePath
        relayAddrs

    unless (null pingErrs) $
      throwCliError (StakePoolCmdRelayPingErrors pingErrs)

stakePoolRelayToAddr
  :: StakePoolRelay
  -> CIO e [Ping.Address (Ping.Unresolved Ping.SRVOrFilePathUnresolved)]
stakePoolRelayToAddr = \case
  StakePoolRelayIp (Just ipv4) Nothing (Just port) ->
    pure [Ping.IP (IPv4 ipv4) (fromIntegral port)]
  StakePoolRelayIp Nothing (Just ipv6) (Just port) ->
    pure [Ping.IP (IPv6 ipv6) (fromIntegral port)]
  StakePoolRelayIp (Just ipv4) (Just ipv6) (Just port) ->
    pure [Ping.IP (IPv6 ipv6) (fromIntegral port), Ping.IP (IPv4 ipv4) (fromIntegral port)]
  -- 'pSingleHostAddress' always parses a port number, so this is unreachable from
  -- the command line.
  relay@(StakePoolRelayIp _ _ Nothing) ->
    throwCliError (StakePoolCmdInvalidRelayError relay)
  -- 'pSingleHostAddress' always parses at least one IP address, so this is
  -- unreachable from the command line.
  relay@(StakePoolRelayIp Nothing Nothing _) ->
    throwCliError (StakePoolCmdInvalidRelayError relay)
  StakePoolRelayDnsARecord dns (Just port) ->
    pure [Ping.mkAddress (BSC.unpack dns ++ ":" ++ show port)]
  StakePoolRelayDnsARecord dns Nothing ->
    pure [Ping.mkAddress (BSC.unpack dns)]
  StakePoolRelayDnsSrvRecord srv ->
    pure [Ping.mkAddress (BSC.unpack srv)]
