{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.Transaction.Command
  ( CompatibleTransactionCmds (..)
  , renderCompatibleTransactionCmd
  )
where

import Cardano.Api
import Cardano.Api.Ledger hiding (TxIn, VotingProcedures)

import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Proposal.Type
import Cardano.CLI.EraBased.Script.Vote.Type
  ( CliVoteScriptRequirements
  )
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

import Data.Text (Text)

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
data CompatibleTransactionCmds era
  = CreateCompatibleSignedTransaction
      (ShelleyBasedEra era)
      [TxIn]
      [TxOutAnyEra]
      !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
      !(Maybe (Featured ConwayEraOnwards era [(ProposalFile In, Maybe CliProposalScriptRequirements)]))
      ![(VoteFile In, Maybe CliVoteScriptRequirements)]
      [WitnessSigningData]
      -- ^ Signing keys
      (Maybe NetworkId)
      !Coin
      -- ^ Tx fee
      ![(CertificateFile, Maybe CliCertificateScriptRequirements)]
      -- ^ stake registering certs
      !(File () Out)

renderCompatibleTransactionCmd :: CompatibleTransactionCmds era -> Text
renderCompatibleTransactionCmd = \case
  CreateCompatibleSignedTransaction{} -> "compatible transaction signed-transaction"
