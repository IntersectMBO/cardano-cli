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
import Cardano.Api.Experimental

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
data CompatibleTransactionCmds era
  = CreateCompatibleSignedTransaction
      (ShelleyBasedEra era)
      [TxIn]
      [TxOutAnyEra]
      !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
      !( Maybe
           (Featured ConwayEraOnwards era [(ProposalFile In, Maybe (ScriptRequirements ProposalItem))])
       )
      ![(VoteFile In, Maybe (ScriptRequirements VoterItem))]
      [WitnessSigningData]
      -- ^ Signing keys
      (Maybe NetworkId)
      !Coin
      -- ^ Tx fee
      ![(CertificateFile, Maybe (ScriptRequirements CertItem))]
      -- ^ stake registering certs
      !(File () Out)

renderCompatibleTransactionCmd :: CompatibleTransactionCmds era -> Text
renderCompatibleTransactionCmd = \case
  CreateCompatibleSignedTransaction{} -> "compatible transaction signed-transaction"
