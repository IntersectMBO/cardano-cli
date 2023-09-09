{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Node
  ( runLegacyNodeCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.Node
import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except (ExceptT)

{- HLINT ignore "Reduce duplication" -}

runLegacyNodeCmds :: ()
  => LegacyNodeCmds
  -> ExceptT NodeCmdError IO ()
runLegacyNodeCmds = \case
  NodeKeyGenCold fmt vk sk ctr ->
    runLegacyNodeKeyGenColdCmd fmt vk sk ctr
  NodeKeyGenKES  fmt vk sk ->
    runLegacyNodeKeyGenKesCmd fmt vk sk
  NodeKeyGenVRF  fmt vk sk ->
    runLegacyNodeKeyGenVrfCmd fmt vk sk
  NodeKeyHashVRF vk mOutFp ->
    runLegacyNodeKeyHashVrfCmd vk mOutFp
  NodeNewCounter vk ctr out ->
    runLegacyNodeNewCounterCmd vk ctr out
  NodeIssueOpCert vk sk ctr p out ->
    runLegacyNodeIssueOpCertCmd vk sk ctr p out

runLegacyNodeKeyGenColdCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenColdCmd = runNodeKeyGenColdCmd

runLegacyNodeKeyGenKesCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenKesCmd = runNodeKeyGenKesCmd

runLegacyNodeKeyGenVrfCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenVrfCmd = runNodeKeyGenVrfCmd

runLegacyNodeKeyHashVrfCmd :: ()
  => VerificationKeyOrFile VrfKey
  -> Maybe (File () Out)
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyHashVrfCmd = runNodeKeyHashVrfCmd

runLegacyNodeNewCounterCmd :: ()
  => ColdVerificationKeyOrFile
  -> Word
  -> OpCertCounterFile InOut
  -> ExceptT NodeCmdError IO ()
runLegacyNodeNewCounterCmd = runNodeNewCounterCmd

runLegacyNodeIssueOpCertCmd :: ()
  => VerificationKeyOrFile KesKey
  -- ^ This is the hot KES verification key.
  -> SigningKeyFile In
  -- ^ This is the cold signing key.
  -> OpCertCounterFile InOut
  -- ^ Counter that establishes the precedence
  -- of the operational certificate.
  -> KESPeriod
  -- ^ Start of the validity period for this certificate.
  -> File () Out
  -> ExceptT NodeCmdError IO ()
runLegacyNodeIssueOpCertCmd = runNodeIssueOpCertCmd
