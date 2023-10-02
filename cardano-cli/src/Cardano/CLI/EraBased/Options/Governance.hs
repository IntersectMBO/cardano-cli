{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance
  ( GovernanceCmds(..)
  , renderGovernanceCmds
  , pGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance.Actions
import           Cardano.CLI.EraBased.Options.Governance.Committee
import           Cardano.CLI.EraBased.Options.Governance.DRep
import           Cardano.CLI.EraBased.Options.Governance.Query
import           Cardano.CLI.EraBased.Options.Governance.Vote
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceCmds era))
pGovernanceCmds era envCli =
  subInfoParser "governance"
    ( Opt.progDesc
        $ mconcat
          [ "Governance commands."
          ]
    )
    [ pCreateMirCertificatesCmds era
    , fmap GovernanceQueryCmds        <$> pGovernanceQueryCmds era envCli
    , fmap GovernanceActionCmds       <$> pGovernanceActionCmds era
    , fmap GovernanceCommitteeCmds    <$> pGovernanceCommitteeCmds era
    , fmap GovernanceDRepCmds         <$> pGovernanceDRepCmds era
    , fmap GovernanceVoteCmds         <$> pGovernanceVoteCmds era
    ]

pCreateMirCertificatesCmds :: CardanoEra era -> Maybe (Parser (GovernanceCmds era))
pCreateMirCertificatesCmds era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create-mir-certificate"
    $ Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w)
    $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
mirCertParsers w =
  asum
    [ subParser "stake-addresses"
      $ Opt.info (pMIRPayStakeAddresses w)
      $ Opt.progDesc "Create an MIR certificate to pay stake addresses"
    , subParser "transfer-to-treasury"
      $ Opt.info (pMIRTransferToTreasury w)
      $ Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
    , subParser "transfer-to-rewards"
      $ Opt.info (pMIRTransferToReserves w)
      $ Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
    ]

pMIRPayStakeAddresses :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pMIRPayStakeAddresses w =
  GovernanceMIRPayStakeAddressesCertificate w
    <$> pMIRPot
    <*> some pStakeAddress
    <*> some pRewardAmt
    <*> pOutputFile

pMIRTransferToTreasury :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pMIRTransferToTreasury w =
  GovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToTreasury

pMIRTransferToReserves :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pMIRTransferToReserves w =
  GovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToReserves
