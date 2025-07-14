{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Compatible.Governance.Types
  ( GovernanceActionProtocolParametersUpdateCmdArgs (..)
  , UpdateProtocolParametersPreConway (..)
  , UpdateProtocolParametersConwayOnwards (..)
  , CostModelsFile (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

data GovernanceActionProtocolParametersUpdateCmdArgs era
  = GovernanceActionProtocolParametersUpdateCmdArgs
  { uppShelleyBasedEra :: !(ShelleyBasedEra era)
  , uppPreConway :: !(Maybe (UpdateProtocolParametersPreConway era))
  , uppConwayOnwards :: !(Maybe (UpdateProtocolParametersConwayOnwards era))
  , uppNewPParams :: !(EraBasedProtocolParametersUpdate era)
  -- ^ New parameters to be proposed. From Alonzo onwards, the type
  -- 'EraBasedProtocolParametersUpdate' also contains cost models. Since all
  -- other protocol parameters are read from command line arguments, whereas
  -- the cost models are read from a file, we separate the cost models from
  -- the rest of the protocol parameters to ease parsing.
  , uppCostModelsFile :: !(Maybe (CostModelsFile era))
  -- ^ The new cost models proposed. See the comment at 'uppNewPParams' for
  -- why this is a separate field.
  , uppFilePath :: !(File () Out)
  }
  deriving Show

data UpdateProtocolParametersPreConway era
  = UpdateProtocolParametersPreConway
  { eon :: !(ShelleyToBabbageEra era)
  , expiryEpoch :: !EpochNo
  , genesisVerificationKeys :: ![VerificationKeyFile In]
  }

deriving instance Show (UpdateProtocolParametersPreConway era)

data UpdateProtocolParametersConwayOnwards era
  = UpdateProtocolParametersConwayOnwards
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddr :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , governanceActionId :: !(Maybe L.GovActionId)
  , constitutionScriptHash :: !(Maybe ScriptHash)
  }

deriving instance Show (UpdateProtocolParametersConwayOnwards era)

data CostModelsFile era
  = CostModelsFile
  { eon :: !(AlonzoEraOnwards era)
  , costModelsFile :: !(File L.CostModels In)
  }
  deriving Show
