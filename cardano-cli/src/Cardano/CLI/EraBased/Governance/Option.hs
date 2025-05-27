{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Governance.Option
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  , pGovernanceCmds
  , pCreateMirCertificatesCmds
  )
where

import Cardano.Api (Convert (..), ShelleyBasedEra, ShelleyToBabbageEra, forShelleyBasedEraMaybeEon)
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Option
import Cardano.CLI.EraBased.Governance.Command
import Cardano.CLI.EraBased.Governance.Committee.Option
import Cardano.CLI.EraBased.Governance.DRep.Option
import Cardano.CLI.EraBased.Governance.Vote.Option
import Cardano.CLI.Parser

import Data.Foldable
import Options.Applicative
import Options.Applicative qualified as Opt

-- First TODO: Change CardanoEra era to ShelleyBasedEra era
-- Second TODO: Return Parser (GovernanceCmds era) because it's not possible
-- for this to return Nothing when it's parameterized on ShelleyBasedEra era
pGovernanceCmds
  :: Exp.IsEra
       era
  => Maybe (Parser (GovernanceCmds era))
pGovernanceCmds =
  subInfoParser
    "governance"
    ( Opt.progDesc $
        mconcat
          [ "Governance commands."
          ]
    )
    [ fmap GovernanceActionCmds <$> pGovernanceActionCmds (convert Exp.useEra)
    , fmap GovernanceCommitteeCmds <$> pGovernanceCommitteeCmds (convert Exp.useEra)
    , fmap GovernanceDRepCmds <$> pGovernanceDRepCmds (convert Exp.useEra)
    , fmap GovernanceVoteCmds <$> pGovernanceVoteCmds (convert Exp.useEra)
    ]

pCreateMirCertificatesCmds :: ShelleyBasedEra era -> Maybe (Parser (GovernanceCmds era))
pCreateMirCertificatesCmds era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "create-mir-certificate" $
        Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w) $
          Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
mirCertParsers w =
  asum
    [ Opt.hsubparser $
        commandWithMetavar "stake-addresses" $
          Opt.info (pMIRPayStakeAddresses w) $
            Opt.progDesc "Create an MIR certificate to pay stake addresses"
    , Opt.hsubparser $
        commandWithMetavar "transfer-to-treasury" $
          Opt.info (pGovernanceCreateMirCertificateTransferToTreasuryCmd w) $
            Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
    , Opt.hsubparser $
        commandWithMetavar "transfer-to-rewards" $
          Opt.info (pGovernanceCreateMirCertificateTransferToReservesCmd w) $
            Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
    ]

pMIRPayStakeAddresses
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pMIRPayStakeAddresses w =
  GovernanceCreateMirCertificateStakeAddressesCmd w
    <$> pMIRPot
    <*> some (pStakeAddress Nothing)
    <*> some pRewardAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pGovernanceCreateMirCertificateTransferToTreasuryCmd w =
  GovernanceCreateMirCertificateTransferToTreasuryCmd w
    <$> pTransferAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToReservesCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pGovernanceCreateMirCertificateTransferToReservesCmd w =
  GovernanceCreateMirCertificateTransferToReservesCmd w
    <$> pTransferAmt
    <*> pOutputFile
