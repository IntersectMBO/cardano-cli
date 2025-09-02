{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Governance.Option
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  , pGovernanceCmds
  )
where

import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Option
import Cardano.CLI.EraBased.Governance.Command
import Cardano.CLI.EraBased.Governance.Committee.Option
import Cardano.CLI.EraBased.Governance.DRep.Option
import Cardano.CLI.EraBased.Governance.Poll.Option
import Cardano.CLI.EraBased.Governance.Vote.Option

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
    [ fmap GovernanceActionCmds <$> pGovernanceActionCmds
    , fmap GovernanceCommitteeCmds <$> pGovernanceCommitteeCmds
    , fmap GovernanceDRepCmds <$> pGovernanceDRepCmds
    , fmap GovernanceVoteCmds <$> pGovernanceVoteCmds
    , fmap GovernancePollCmds <$> pGovernancePollCmds
    ]
