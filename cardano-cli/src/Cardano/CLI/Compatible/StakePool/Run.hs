{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.StakePool.Run
  ( runCompatibleStakePoolCmds
  , stakePoolRelayToAddr
  )
where

import Cardano.Api
import Cardano.Api.Compatible.Certificate
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Certificate
  ( StakePoolParameters (..)
  , StakePoolRelay (..)
  , toShelleyPoolParams
  )

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.StakePool.Command
import Cardano.CLI.EraBased.StakePool.Internal.Metadata
import Cardano.CLI.Read
  ( getVerificationKeyFromStakePoolVerificationKeySource
  )
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Key (readVerificationKeyOrFile)
import Cardano.Network.Ping qualified as Ping

import Control.Monad
import Control.Tracer (nullTracer, (>$<))
import Data.ByteString.Char8 qualified as BSC
import Data.IP (IP (..))

runCompatibleStakePoolCmds
  :: ()
  => CompatibleStakePoolCmds era
  -> CIO e ()
runCompatibleStakePoolCmds = \case
  CompatibleStakePoolRegistrationCertificateCmd args -> runStakePoolRegistrationCertificateCmd args

runStakePoolRegistrationCertificateCmd
  :: ()
  => CompatibleStakePoolRegistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolRegistrationCertificateCmd
  CompatibleStakePoolRegistrationCertificateCmdArgs
    { sbe = sbe :: ShelleyBasedEra era
    , poolVerificationKeyOrFile
    , vrfVerificationKeyOrFile
    , poolPledge
    , poolCost
    , poolMargin
    , rewardStakeVerificationKeyOrFile
    , ownerStakeVerificationKeyOrFiles
    , relays
    , mMetadata
    , network
    , outFile
    } =
    shelleyBasedEraConstraints sbe $ do
      let relayAddrs = concatMap stakePoolRelayToAddr relays
          pingOpts =
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
      -- Skip the ping when there are no relays to check: 'Ping.pingClients'' builds a
      -- DNS resolver from /etc/resolv.conf before it looks at its address list, so it
      -- fails outright on hosts without one.
      pingErrs <-
        if null relayAddrs
          then pure []
          else liftIO $ do
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

      -- Pool verification key
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <-
        readVerificationKeyOrFile vrfVerificationKeyOrFile
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        readVerificationKeyOrFile rewardStakeVerificationKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress network stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <- forM ownerStakeVerificationKeyOrFiles readVerificationKeyOrFile
      let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

      let stakePoolParams =
            StakePoolParameters
              { stakePoolId = stakePoolId'
              , stakePoolVRF = vrfKeyHash'
              , stakePoolCost = poolCost
              , stakePoolMargin = poolMargin
              , stakePoolRewardAccount = rewardAccountAddr
              , stakePoolPledge = poolPledge
              , stakePoolOwners = stakePoolOwners'
              , stakePoolRelays = relays
              , stakePoolMetadata = pcaAnchor <$> mMetadata
              }

      let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
          registrationCert =
            makeStakePoolRegistrationCertificate ledgerStakePoolParams
              :: Exp.Certificate (ShelleyLedgerEra era)

      mapM_ (fromExceptTCli . carryHashChecks) mMetadata

      fromExceptTCli
        . firstExceptT StakePoolCmdWriteFileError
        . newExceptT
        . writeLazyByteStringFile outFile
        $ textEnvelopeToJSON (Just registrationCertDesc) registrationCert
   where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

stakePoolRelayToAddr
  :: StakePoolRelay
  -> [Ping.Address (Ping.Unresolved Ping.SRVOrFilePathUnresolved)]
stakePoolRelayToAddr (StakePoolRelayIp (Just ipv4) Nothing (Just port)) = [Ping.IP (IPv4 ipv4) (fromIntegral port)]
stakePoolRelayToAddr (StakePoolRelayIp Nothing (Just ipv6) (Just port)) = [Ping.IP (IPv6 ipv6) (fromIntegral port)]
stakePoolRelayToAddr (StakePoolRelayIp (Just ipv4) (Just ipv6) (Just port)) = [Ping.IP (IPv6 ipv6) (fromIntegral port), Ping.IP (IPv4 ipv4) (fromIntegral port)]
-- the pSingHostAddress parser always includes a port number
stakePoolRelayToAddr (StakePoolRelayIp _ _ Nothing) = error "unexpected happend"
-- the pSingHostAddress parser always includes at least one ip address
stakePoolRelayToAddr (StakePoolRelayIp Nothing Nothing _) = error "unexpected happend"
stakePoolRelayToAddr (StakePoolRelayDnsARecord dns (Just port)) = [Ping.mkAddress (BSC.unpack dns ++ ":" ++ show port)]
stakePoolRelayToAddr (StakePoolRelayDnsARecord dns Nothing) = [Ping.mkAddress (BSC.unpack dns)]
stakePoolRelayToAddr (StakePoolRelayDnsSrvRecord srv) = [Ping.mkAddress (BSC.unpack srv)]
