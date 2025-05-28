{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.Governance.Run
  ( runCompatibleGovernanceCmds
  )
where

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
import Cardano.CLI.EraBased.Governance.Run

import Data.Typeable (Typeable)

runCompatibleGovernanceCmds :: Typeable era => CompatibleGovernanceCmds era -> CIO e ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolParametersUpdateCmd cmd ->
    runGovernanceActionCmds cmd
  LatestCompatibleGovernanceCmds cmd -> runGovernanceCmds cmd
  CompatibleGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out
  CompatibleCreateMirCertificateStakeAddressesCmd w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  CompatibleCreateMirCertificateTransferToReservesCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp
  CompatibleCreateMirCertificateTransferToTreasuryCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp
