module Cardano.CLI.EraBased.Options.Governance.Actions
  ( pGovernanceActionCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import qualified Cardano.CLI.EraBased.Legacy as Legacy
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceActionCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionCmds era =
  subInfoParser "action"
    ( Opt.progDesc
        $ mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitution era
    , pGovernanceActionCreateUpdateProposal era
    ]


pGovernanceActionNewConstitution
  :: CardanoEra era  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNewConstitution =
  featureInEra Nothing (\cOn -> Just $
     subParser "create-constitution"
        $ Opt.info (pCmd cOn)
        $ Opt.progDesc "Create a constitution.")
 where
  pCmd :: ConwayEraOnwards era -> Parser (GovernanceActionCmds era)
  pCmd cOn =
    fmap (GovernanceActionCreateConstitution cOn)  $
      EraBasedNewConstitution
        <$> pGovActionDeposit
        <*> pAnyStakeIdentifier
        <*> pConstitution
        <*> pFileOutDirection "out-file" "Output filepath of the constitution."

pAnyStakeIdentifier :: Parser AnyStakeIdentifier
pAnyStakeIdentifier =
  asum [ AnyStakePoolKey <$> pStakePoolVerificationKeyOrHashOrFile
       , AnyStakeKey <$> pStakeVerificationKeyOrHashOrFile
       ]

pGovernanceActionCreateUpdateProposal :: CardanoEra era -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionCreateUpdateProposal =
  featureInEra Nothing (\cOn -> Just $
     subParser "create-update-proposal"
        $ Opt.info (pCmd cOn)
        $ Opt.progDesc "Create an update proposal.")
 where
  pCmd :: ConwayEraOnwards era -> Parser (GovernanceActionCmds era)
  pCmd cOn =
    fmap (GovernanceActionCreateUpdateProposal cOn) $
      ConwayOnwardsUpdateProposal cOn
        <$> Legacy.pProtocolParametersUpdate
        <*> pGovActionDeposit
        <*> pAnyStakeIdentifier
        <*> optional (pFileInDirection "cost-model" "Cost model filepath.")
        <*> pFileOutDirection "out-file" "Output filepath of the update proposal."
