{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance
  ( EraBasedGovernanceCmd(..)
  , renderEraBasedGovernanceCmd
  , pEraBasedGovernanceCmd
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.EraBased.Legacy
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative
import qualified Options.Applicative as Opt

data EraBasedGovernanceCmd era
  = EraBasedGovernancePreConwayCmd (ShelleyToBabbageEra era)
  | EraBasedGovernancePostConwayCmd (ConwayEraOnwards era)
  | EraBasedGovernanceMIRPayStakeAddressesCertificate
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | EraBasedGovernanceMIRTransfer
      (ShelleyToBabbageEra era)
      Lovelace
      (File () Out)
      TransferDirection
  | EraBasedGovernanceDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | EraBasedGovernanceRegistrationCertificateCmd
      AnyRegistrationTarget
      (File () Out)
  | EraBasedGovernanceVoteCmds
      GovernanceVoteCmds

renderEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> Text
renderEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd {} -> "governance pre-conway"
  EraBasedGovernancePostConwayCmd {} -> "governance post-conway"
  EraBasedGovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
  EraBasedGovernanceDelegationCertificateCmd {} -> "governance delegation-certificate"
  EraBasedGovernanceRegistrationCertificateCmd {} -> "governance registration-certificate"
  EraBasedGovernanceVoteCmds cmds -> renderGovernanceVoteCmds cmds

pEraBasedGovernanceCmd :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmd era)
pEraBasedGovernanceCmd envCli era =
  asum $ catMaybes
    [ pEraBasedRegistrationCertificateCmd envCli era
    , pEraBasedDelegationCertificateCmd envCli era
    , pCreateMirCertificatesCmds era
    , pEraBasedVoteCmds envCli era
    ]


-- Registration Certificate related


pEraBasedRegistrationCertificateCmd
  :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pEraBasedRegistrationCertificateCmd envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "registration-certificate"
      $ Opt.info (pEraCmd envCli w)
      $ Opt.progDesc "Create a registration certificate."
 where
  pEraCmd :: EnvCli -> AnyEraDecider era -> Parser (EraBasedGovernanceCmd era)
  pEraCmd envCli' = \case
    AnyEraDeciderShelleyToBabbage sToB ->
      EraBasedGovernanceRegistrationCertificateCmd
        <$> asum [ ShelleyToBabbageStakePoolRegTarget sToB
                     <$> pStakePoolRegistrationParserRequirements envCli'
                 , ShelleyToBabbageStakeKeyRegTarget sToB
                     <$> pStakeIdentifier
                 ]
        <*> pOutputFile

    AnyEraDeciderConwayOnwards cOn ->
      EraBasedGovernanceRegistrationCertificateCmd . ConwayOnwardRegTarget cOn
        <$> asum [ RegisterStakePool cOn
                     <$> pStakePoolRegistrationParserRequirements envCli'
                 , RegisterStakeKey cOn
                     <$> pStakeIdentifier
                     <*> pKeyRegistDeposit
                 , RegisterDRep cOn
                     <$> pDRepVerificationKeyOrHashOrFile
                     <*> pKeyRegistDeposit
                 ]
        <*> pOutputFile



--------------------------------------------------------------------------------


data AnyEraDecider era where
  AnyEraDeciderShelleyToBabbage :: ShelleyToBabbageEra era -> AnyEraDecider era
  AnyEraDeciderConwayOnwards :: ConwayEraOnwards era -> AnyEraDecider era

instance FeatureInEra AnyEraDecider where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraShelley
    AllegraEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAllegra
    MaryEra     -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraMary
    AlonzoEra   -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraBabbage
    ConwayEra   -> yes $ AnyEraDeciderConwayOnwards ConwayEraOnwardsConway

pEraBasedVoteCmds :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (EraBasedGovernanceCmd era))
pEraBasedVoteCmds envCli _ =
  Just
    $ subParser "vote"
    $ Opt.info (EraBasedGovernanceVoteCmds <$> pVoteCommmands envCli)
    $ Opt.progDesc "Vote related commands."

-- Delegation Certificate related
pEraBasedDelegationCertificateCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pEraBasedDelegationCertificateCmd _envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "delegation-certificate"
      $ Opt.info (pCmd w)
      $ Opt.progDesc "Delegation certificate creation."
 where
  pCmd :: AnyEraDecider era -> Parser (EraBasedGovernanceCmd era)
  pCmd w =
    EraBasedGovernanceDelegationCertificateCmd
      <$> pStakeIdentifier
      <*> pAnyDelegationCertificateTarget w
      <*> pOutputFile

  pAnyDelegationCertificateTarget :: ()
    => AnyEraDecider era
    -> Parser AnyDelegationTarget
  pAnyDelegationCertificateTarget e =
    case e of
      AnyEraDeciderShelleyToBabbage sbe ->
        ShelleyToBabbageDelegTarget sbe
          <$> pStakePoolVerificationKeyOrHashOrFile
      AnyEraDeciderConwayOnwards cOnwards ->
        ConwayOnwardDelegTarget cOnwards
          <$> pStakeTarget cOnwards

-- TODO: Conway era AFTER sancho net. We probably want to
-- differentiate between delegating voting stake and reward stake
pStakeTarget :: ConwayEraOnwards era -> Parser (StakeTarget era)
pStakeTarget cOnwards =
  asum
    [ TargetStakePool cOnwards <$> pStakePoolVerificationKeyOrHashOrFile
    , TargetVotingDrep cOnwards <$> pDRepVerificationKeyOrHashOrFile
    , TargetVotingDrepAndStakePool cOnwards
         <$> pDRepVerificationKeyOrHashOrFile
         <*> pStakePoolVerificationKeyOrHashOrFile
    ]

pDRepVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile DRepKey)
pDRepVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pDRepVerificationKeyOrFile
    , VerificationKeyHash <$> pDRepVerificationKeyHash
    ]

pDRepVerificationKeyHash :: Parser (Hash DRepKey)
pDRepVerificationKeyHash =
    Opt.option (pBech32KeyHash AsDRepKey <|> pHexKeyHash AsDRepKey) $ mconcat
      [ Opt.long "drep-key-hash"
      , Opt.metavar "HASH"
      , Opt.help $ mconcat
          [ "DRep verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pDRepVerificationKey :: Parser (VerificationKey DRepKey)
pDRepVerificationKey =
  Opt.option (readVerificationKey AsDRepKey) $ mconcat
    [ Opt.long "drep-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "DRep verification key (Bech32 or hex-encoded)."
    ]

pDRepVerificationKeyOrFile :: Parser (VerificationKeyOrFile DRepKey)
pDRepVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pDRepVerificationKey
    , VerificationKeyFilePath <$> pDRepVerificationKeyFile
    ]

pDRepVerificationKeyFile :: Parser (VerificationKeyFile In)
pDRepVerificationKeyFile =
  fmap File . Opt.strOption $ mconcat
    [ Opt.long "drep-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the DRep verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

--------------------------------------------------------------------------------

pCreateMirCertificatesCmds :: CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pCreateMirCertificatesCmds =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "create-mir-certificate"
      $ Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w)
      $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
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
  -> Parser (EraBasedGovernanceCmd era)
pMIRPayStakeAddresses w =
  EraBasedGovernanceMIRPayStakeAddressesCertificate w
    <$> pMIRPot
    <*> some pStakeAddress
    <*> some pRewardAmt
    <*> pOutputFile

pMIRTransferToTreasury :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
pMIRTransferToTreasury w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToTreasury

pMIRTransferToReserves :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
pMIRTransferToReserves w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToReserves


