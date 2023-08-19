{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.DRep
  ( runGovernanceDRepCmds
  ) where

import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Run.Certificate
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.Types.Errors.CmdError

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function

runGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    runGovernanceDelegationCertificate stakeIdentifier delegationTarget outFp
      & firstExceptT CmdEraDelegationError

  GovernanceDRepRegistrationCertificateCmd regTarget outFp ->
    runGovernanceRegistrationCertificate regTarget outFp
      & firstExceptT CmdEraBasedRegistrationError

  GovernanceDRepGenerateKey w vrf sgn ->
    runGovernanceDRepKeyGen w vrf sgn
      & firstExceptT CmdGovernanceCmdError
