{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.EraBased.Options.Governance.DRep
  ( pGovernanceDRepCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as L

import           Control.Applicative
import           Data.Foldable
import           Data.String
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceDRepCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds era =
  subInfoParser "drep"
    ( Opt.progDesc
        $ mconcat
          [ "DRep member commands."
          ]
    )
    [ pGovernanceDRepKeyGenCmd era
    , pGovernanceDRepKeyIdCmd era
    , pRegistrationCertificateCmd era
    ]

pGovernanceDRepKeyGenCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGenCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "key-gen"
    $ Opt.info
        ( GovernanceDRepGenerateKeyCmd w
            <$> pVerificationKeyFileOut
            <*> pSigningKeyFileOut
        )
    $ Opt.progDesc "Generate Delegate Representative verification and signing keys."

pGovernanceDRepKeyIdCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyIdCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "id"
    $ Opt.info
        ( GovernanceDRepIdCmd w
            <$> pDRepVerificationKeyOrFile
            <*> pDRepIdOutputFormat
            <*> optional pOutputFile
        )
    $ Opt.progDesc "Generate a drep id."

pDRepIdOutputFormat :: Parser IdOutputFormat
pDRepIdOutputFormat =
  Opt.option readIdOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional drep id output format. Accepted output formats are \"hex\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value IdOutputFormatBech32
    ]

-- Registration Certificate related

pRegistrationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pRegistrationCertificateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "registration-certificate"
    $ Opt.info (conwayEraOnwardsConstraints w $ mkParser w)
    $ Opt.progDesc "Create a registration certificate."
 where
  mkParser w = GovernanceDRepRegistrationCertificateCmd w
        <$> pDRepVerificationKeyOrHashOrFile
        <*> pKeyRegistDeposit
        <*> pDRepMetadata
        <*> pOutputFile

pDRepMetadata :: Parser (Maybe (L.Anchor Crypto.StandardCrypto))
pDRepMetadata =
  optional $
    L.Anchor
      <$> fmap unAnchorUrl pDrepMetadataUrl
      <*> pDrepMetadataHash

pDrepMetadataUrl :: Parser AnchorUrl
pDrepMetadataUrl =
  AnchorUrl
    <$> pUrl "drep-metadata-url" "DRep anchor URL"

pDrepMetadataHash :: Parser (L.SafeHash Crypto.StandardCrypto L.AnchorData)
pDrepMetadataHash =
  Opt.option readSafeHash $ mconcat
    [ Opt.long "drep-metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "DRep anchor data hash."
    ]


--------------------------------------------------------------------------------

data AnyEraDecider era where
  AnyEraDeciderShelleyToBabbage :: ShelleyToBabbageEra era -> AnyEraDecider era
  AnyEraDeciderConwayOnwards :: ConwayEraOnwards era -> AnyEraDecider era

instance Eon AnyEraDecider where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraShelley
    AllegraEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAllegra
    MaryEra     -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraMary
    AlonzoEra   -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraBabbage
    ConwayEra   -> yes $ AnyEraDeciderConwayOnwards ConwayEraOnwardsConway
