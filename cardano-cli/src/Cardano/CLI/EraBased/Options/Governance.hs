{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  , pGovernanceCmds
  )
where

import           Cardano.Api (CardanoEra, ShelleyToBabbageEra, forEraMaybeEon)

import           Cardano.CLI.EraBased.Commands.Governance
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance.Actions
import           Cardano.CLI.EraBased.Options.Governance.Committee
import           Cardano.CLI.EraBased.Options.Governance.DRep
import           Cardano.CLI.EraBased.Options.Governance.Poll
import           Cardano.CLI.EraBased.Options.Governance.Vote
import           Cardano.CLI.Parser

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

-- First TODO: Change CardanoEra era to ShelleyBasedEra era
-- Second TODO: Return Parser (GovernanceCmds era) because it's not possible
-- for this to return Nothing when it's parameterized on ShelleyBasedEra era
pGovernanceCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceCmds era))
pGovernanceCmds era =
  subInfoParser
    "governance"
    ( Opt.progDesc $
        mconcat
          [ "Governance commands."
          ]
    )
    [ pCreateMirCertificatesCmds era
    , pGovernanceGenesisKeyDelegationCertificate era
    , fmap GovernanceActionCmds <$> pGovernanceActionCmds era
    , fmap GovernanceCommitteeCmds <$> pGovernanceCommitteeCmds era
    , fmap GovernanceDRepCmds <$> pGovernanceDRepCmds era
    , fmap GovernancePollCmds <$> pGovernancePollCmds era
    , fmap GovernanceVoteCmds <$> pGovernanceVoteCmds era
    ]

pCreateMirCertificatesCmds :: ShelleyBasedEra era -> Maybe (Parser (GovernanceCmds era))
pCreateMirCertificatesCmds era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    subParser "create-mir-certificate" $
      Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w) $
        Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
mirCertParsers w =
  asum
    [ subParser "stake-addresses" $
        Opt.info (pMIRPayStakeAddresses w) $
          Opt.progDesc "Create an MIR certificate to pay stake addresses"
    , subParser "transfer-to-treasury" $
        Opt.info (pGovernanceCreateMirCertificateTransferToTreasuryCmd w) $
          Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
    , subParser "transfer-to-rewards" $
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

pGovernanceGenesisKeyDelegationCertificate
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceCmds era))
pGovernanceGenesisKeyDelegationCertificate era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    subParser "create-genesis-key-delegation-certificate" $
      Opt.info (parser w) $
        Opt.progDesc "Create a genesis key delegation certificate"
 where
  parser w =
    GovernanceGenesisKeyDelegationCertificate w
      <$> pGenesisVerificationKeyOrHashOrFile
      <*> pGenesisDelegateVerificationKeyOrHashOrFile
      <*> pVrfVerificationKeyOrHashOrFile
      <*> pOutputFile
