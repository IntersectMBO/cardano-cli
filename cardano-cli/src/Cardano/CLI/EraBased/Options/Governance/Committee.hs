{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Options.Governance.Committee
  ( pGovernanceCommitteeCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.EraBased.Options.Common hiding (pAnchorUrl)
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Key

import           Data.Foldable (asum)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceCommitteeCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCmds era =
  subInfoParser "committee"
    ( Opt.progDesc
        $ mconcat
          [ "Committee member commands."
          ]
    )
    [ pGovernanceCommitteeKeyGenColdCmd era
    , pGovernanceCommitteeKeyGenHotCmd era
    , pGovernanceCommitteeKeyHashCmd era
    , pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd era
    , pGovernanceCommitteeCreateColdKeyResignationCertificateCmd era
    ]

pGovernanceCommitteeKeyGenColdCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenColdCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "key-gen-cold"
    $ Opt.info (pCmd w)
    $ Opt.progDesc
    $ mconcat
        [ "Create a cold key pair for a Constitutional Committee Member"
        ]
  where
    pCmd :: ()
      => ConwayEraOnwards era
      -> Parser (GovernanceCommitteeCmds era)
    pCmd w =
      fmap GovernanceCommitteeKeyGenColdCmd $
        GovernanceCommitteeKeyGenColdCmdArgs w
          <$> pColdVerificationKeyFile
          <*> pColdSigningKeyFile

pGovernanceCommitteeKeyGenHotCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenHotCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "key-gen-hot"
    $ Opt.info (pCmd w)
    $ Opt.progDesc
    $ mconcat
        [ "Create a hot key pair for a Constitutional Committee Member"
        ]
  where
    pCmd :: ()
      => ConwayEraOnwards era
      -> Parser (GovernanceCommitteeCmds era)
    pCmd w =
      fmap GovernanceCommitteeKeyGenHotCmd $
        GovernanceCommitteeKeyGenHotCmdArgs w
          <$> pVerificationKeyFileOut
          <*> pSigningKeyFileOut

pGovernanceCommitteeKeyHashCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyHashCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "key-hash"
    $ Opt.info
        ( fmap GovernanceCommitteeKeyHashCmd $
            GovernanceCommitteeKeyHashCmdArgs w
              <$> pAnyVerificationKeySource "Constitutional Committee Member key (hot or cold)"
        )
    $ Opt.progDesc
    $ mconcat
        [ "Print the identifier (hash) of a public key"
        ]

pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create-hot-key-authorization-certificate"
    $ Opt.info
        ( fmap GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd $
            GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs w
              <$> pColdCredential
              <*> pHotVerificationKeyOrHashOrFileOrScript
              <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create hot key authorization certificate for a Constitutional Committee Member"
        ]
  where
    pHotVerificationKeyOrHashOrFileOrScript = asum
      [ VkhfsKeyHashFile <$> pCommitteeHotKeyOrHashOrFile
      , VkhfsScript <$> pScriptFor "hot-script-file" Nothing "Hot Native or Plutus script file"
      ]

pGovernanceCommitteeCreateColdKeyResignationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateColdKeyResignationCertificateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create-cold-key-resignation-certificate"
    $ Opt.info (conwayEraOnwardsConstraints w $ mkParser w)
    $ Opt.progDesc
    $ mconcat
        [ "Create cold key resignation certificate for a Constitutional Committee Member"
        ]
 where
  mkParser w =
    GovernanceCommitteeCreateColdKeyResignationCertificateCmd <$>
      (GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs w <$>
        pColdCredential <*> pAnchor <*> pOutputFile)

pColdCredential :: Parser (VerificationKeySource CommitteeColdKey)
pColdCredential =
  asum
    [ VksKeyHashFile . VerificationKeyOrFile <$> pCommitteeColdVerificationKeyOrFile
    , VksKeyHashFile . VerificationKeyHash <$> pCommitteeColdVerificationKeyHash
    , VksScriptHash <$>
        pScriptHash
          "cold-script-hash"
          "Committee cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    , VksScript <$> pScriptFor "cold-script-file" Nothing "Cold Native or Plutus script file"
    ]

pAnchor :: Parser (Maybe (L.Anchor L.StandardCrypto))
pAnchor =
  Opt.optional $
    L.Anchor
      <$> fmap unAnchorUrl pAnchorUrl
      <*> pSafeHash

pAnchorUrl :: Parser AnchorUrl
pAnchorUrl =
  AnchorUrl
    <$> pUrl "resignation-metadata-url" "Constitutional Committee cold key resignation certificate URL"

pSafeHash ::  Parser (L.SafeHash L.StandardCrypto L.AnchorData)
pSafeHash =
  Opt.option readSafeHash $ mconcat
    [ Opt.long "resignation-metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Constitutional Committee cold key resignation certificate metadata hash"
    ]
