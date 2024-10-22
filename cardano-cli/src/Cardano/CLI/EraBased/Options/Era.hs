module Cardano.CLI.EraBased.Options.Era
  ( pAnyEraCommand
  )
where

import           Cardano.Api (ShelleyBasedEra (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.TopLevelCommands
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.TopLevelCommands
import           Cardano.CLI.Parser

import           Data.Foldable
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      subParser "shelley" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraShelley <$> pCmds ShelleyBasedEraShelley envCli) $
          Opt.progDesc ("Shelley era commands" <> deprecationText)
    , subParser "allegra" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraAllegra <$> pCmds ShelleyBasedEraAllegra envCli) $
          Opt.progDesc ("Allegra era commands" <> deprecationText)
    , subParser "mary" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraMary <$> pCmds ShelleyBasedEraMary envCli) $
          Opt.progDesc ("Mary era commands" <> deprecationText)
    , subParser "alonzo" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraAlonzo <$> pCmds ShelleyBasedEraAlonzo envCli) $
          Opt.progDesc ("Alonzo era commands" <> deprecationText)
    , subParser "babbage" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds ShelleyBasedEraBabbage envCli) $
          Opt.progDesc ("Babbage era commands" <> deprecationText)
    , subParser "conway" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli) $
          Opt.progDesc "Conway era commands"
    , subParser "latest" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli) $
          Opt.progDesc "Latest era commands (Conway)"
    ]
