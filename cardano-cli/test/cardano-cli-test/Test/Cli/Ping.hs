-- | @cardano-cli ping@'s parser is a local mirror of
-- @Cardano.Network.Ping.cmdlineParser@ from @cardano-diffusion:ping@, which
-- it replaces (see 'Cardano.CLI.EraIndependent.Ping.Option.pPing').  These
-- tests pin the library parser's behaviour: defaults, option readers,
-- address parsing and rejected command lines.
module Test.Cli.Ping
  ( hprop_ping_parser_options
  , hprop_ping_parser_addresses
  , hprop_ping_parser_failures
  )
where

import Cardano.CLI.EraIndependent.Ping.Command (PingCmd (..))
import Cardano.CLI.EraIndependent.Ping.Option (pPing)
import Cardano.Network.Ping qualified as Ping

import Data.Maybe (isNothing)
import Data.Word (Word32)
import Options.Applicative qualified as Opt

import Test.Cardano.CLI.Util (watchdogProp)

import Hedgehog (Property, annotateShow, assert, (===))
import Hedgehog.Extras (propertyOnce)

-- | 'Ping.PingOpts' and 'Ping.HashType' have no 'Eq' or 'Show' instances,
-- so parse results are compared (and reported) through this projection.
data ProjectedOpts = ProjectedOpts
  { count :: Word32
  , magic :: Word32
  , json :: Ping.LogFormat
  , quiet :: Bool
  , mode :: Ping.PingMode
  , srvPrefix :: String
  , color :: Ping.ColorMode
  , shortHash :: Bool
  }
  deriving (Eq, Show)

projectOpts :: Ping.PingOpts -> ProjectedOpts
projectOpts opts =
  ProjectedOpts
    { count = Ping.pingOptsCount opts
    , magic = Ping.unNetworkMagic (Ping.pingOptsMagic opts)
    , json = Ping.pingOptsJson opts
    , quiet = Ping.pingOptsQuiet opts
    , mode = Ping.pingOptsMode opts
    , srvPrefix = Ping.pingOptsSRVPrefix opts
    , color = Ping.pingOptsColor opts
    , shortHash =
        case Ping.pingOptsHashType opts of
          Ping.ShortHash -> True
          Ping.FullHash -> False
    }

parsePing :: [String] -> Maybe (ProjectedOpts, [String])
parsePing args = do
  cmd <-
    Opt.getParseResult $
      Opt.execParserPure Opt.defaultPrefs (Opt.info pPing mempty) args
  pure (projectOpts (pingOpts cmd), show <$> pingAddresses cmd)

defaultOpts :: ProjectedOpts
defaultOpts =
  ProjectedOpts
    { count = maxBound
    , magic = 764824073 -- mainnet magic
    , json = Ping.AsText
    , quiet = False
    , mode = Ping.PingMode
    , srvPrefix = "_cardano._tcp"
    , color = Ping.ColorAuto
    , shortHash = False
    }

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/ping parser options/"'@
hprop_ping_parser_options :: Property
hprop_ping_parser_options = watchdogProp . propertyOnce $ do
  parseOpts [] === Just defaultOpts
  parseOpts ["--count", "7"] === Just defaultOpts{count = 7}
  parseOpts ["-c", "7"] === Just defaultOpts{count = 7}
  parseOpts ["--network-magic", "2"] === Just defaultOpts{magic = 2}
  parseOpts ["-m", "2"] === Just defaultOpts{magic = 2}
  parseOpts ["--json"] === Just defaultOpts{json = Ping.AsJSON}
  parseOpts ["-j"] === Just defaultOpts{json = Ping.AsJSON}
  parseOpts ["--quiet"] === Just defaultOpts{quiet = True}
  parseOpts ["-q"] === Just defaultOpts{quiet = True}
  parseOpts ["--mode", "ping"] === Just defaultOpts{mode = Ping.PingMode}
  parseOpts ["--mode", "tip"] === Just defaultOpts{mode = Ping.TipMode}
  parseOpts ["--mode", "query"] === Just defaultOpts{mode = Ping.QueryMode}
  parseOpts ["--srv-prefix", "_test._tcp"] === Just defaultOpts{srvPrefix = "_test._tcp"}
  parseOpts ["--color", "auto"] === Just defaultOpts{color = Ping.ColorAuto}
  parseOpts ["--color", "never"] === Just defaultOpts{color = Ping.ColorNever}
  parseOpts ["--color", "always"] === Just defaultOpts{color = Ping.ColorAlways}
  parseOpts ["--short-hash"] === Just defaultOpts{shortHash = True}
 where
  parseOpts args = fst <$> parsePing (args <> ["127.0.0.1:3001"])

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/ping parser addresses/"'@
hprop_ping_parser_addresses :: Property
hprop_ping_parser_addresses = watchdogProp . propertyOnce $ do
  -- IP literals with a port parse to `IP` addresses
  parseAddresses ["127.0.0.1:3001"] === Just ["IP 127.0.0.1 3001"]
  parseAddresses ["[::1]:3001"] === Just ["IP ::1 3001"]
  -- anything with a colon, slash or without a dot may be a domain with
  -- a port or a file path, resolved at runtime
  parseAddresses ["example.org:3001"] === Just ["FilePathOrDomain \"example.org:3001\""]
  parseAddresses ["/tmp/node.socket"] === Just ["FilePathOrDomain \"/tmp/node.socket\""]
  parseAddresses ["socket"] === Just ["FilePathOrDomain \"socket\""]
  -- a dotted name without a port is looked up as an SRV record first
  parseAddresses ["example.org"] === Just ["SRV \"example.org\""]
  parseAddresses ["node.sock"] === Just ["SRV \"node.sock\""]
  -- multiple addresses are accepted
  parseAddresses ["127.0.0.1:3001", "example.org", "[::1]:3001"]
    === Just ["IP 127.0.0.1 3001", "SRV \"example.org\"", "IP ::1 3001"]
 where
  parseAddresses args = snd <$> parsePing args

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/ping parser failures/"'@
hprop_ping_parser_failures :: Property
hprop_ping_parser_failures = watchdogProp . propertyOnce $ do
  mapM_
    rejected
    [ -- at least one address is required
      []
    , ["--count", "7"]
    , -- invalid option values
      ["--mode", "bogus", "127.0.0.1:3001"]
    , ["--color", "bogus", "127.0.0.1:3001"]
    , ["--count", "bogus", "127.0.0.1:3001"]
    , -- the pre-11.2 interface is gone
      ["--host", "relay.iohk.example"]
    , ["--unixsock", "node.socket"]
    , ["-u", "node.socket"]
    , ["--port", "3001", "127.0.0.1:3001"]
    , ["--magic", "42", "127.0.0.1:3001"]
    , ["--tip", "127.0.0.1:3001"]
    , ["-t", "127.0.0.1:3001"]
    , ["--query-versions", "127.0.0.1:3001"]
    , ["-Q", "127.0.0.1:3001"]
    ]
 where
  rejected args = do
    annotateShow args
    assert $ isNothing (parsePing args)
