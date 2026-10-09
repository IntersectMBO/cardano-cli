{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Ping.Option
  ( parsePingCmd
  , pPing
  )
where

import Cardano.CLI.Command (ClientCommand (CliPingCommand))
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as Ping

import Control.Applicative
import Data.IP (IP)
import Options.Applicative qualified as Opt
import Options.Applicative.Help.Pretty qualified as Pretty
import Prettyprinter qualified as PP
import Text.Read (readMaybe)

parsePingCmd :: Opt.Mod Opt.CommandFields ClientCommand
parsePingCmd =
  Opt.command "ping" $
    Opt.info (CliPingCommand <$> pPing <**> Opt.helper) $
      Opt.progDescDoc $
        Just $
          mconcat
            [ PP.pretty @String "Ping a cardano node either using node-to-node or node-to-client protocol. "
            , PP.pretty @String "It negotiates a handshake and keeps sending keep alive messages."
            ]

-- | A local mirror of @Cardano.Network.Ping.cmdlineParser@ from
-- @cardano-diffusion:ping@, which cardano-cli used directly before.  The
-- library parser is built against vanilla @optparse-applicative@ by default,
-- while cardano-cli uses @optparse-applicative-fork@; using it required
-- building @cardano-diffusion@ with a non-default cabal flag set via
-- @cabal.project@, which does not ship with the sdist.
--
-- This parser must behave exactly like @cmdlineParser@; it is a verbatim
-- copy modulo qualification.  'Test.Cli.Ping' checks the equivalence of the
-- two parsers, and the golden help tests pin the rendered help text.
pPing :: Opt.Parser PingCmd
pPing = PingCmd <$> pPingOpts <*> pPingAddresses

-- | A copy of @Cardano.Network.Ping.pingOptsParser@.
pPingOpts :: Opt.Parser Ping.PingOpts
pPingOpts =
  Ping.PingOpts
    <$> Opt.option
      Opt.auto
      ( Opt.long "count"
          <> Opt.short 'c'
          <> Opt.help
            ( mconcat
                [ "Stop after sending count requests and receiving count responses.  "
                , "If this option is not specified, ping will operate until interrupted.  "
                ]
            )
          <> Opt.metavar "COUNT"
          <> Opt.value maxBound
          <> Opt.showDefault
      )
    <*> Opt.option
      (Ping.NetworkMagic <$> Opt.auto)
      ( Opt.long "network-magic"
          <> Opt.short 'm'
          <> Opt.help "Network magic."
          <> Opt.value Ping.mainnetMagic
          <> Opt.metavar "MAGIC"
          <> Opt.showDefaultWith (show . Ping.unNetworkMagic)
      )
    <*> Opt.flag
      Ping.AsText
      Ping.AsJSON
      ( Opt.long "json"
          <> Opt.short 'j'
          <> Opt.help "JSON output flag."
      )
    <*> Opt.flag
      False
      True
      ( Opt.long "quiet"
          <> Opt.short 'q'
          <> Opt.help "Quiet flag, CSV/JSON only output."
      )
    <*> Opt.option
      pingMode
      ( Opt.long "mode"
          <> Opt.helpDoc
            ( Just $
                Pretty.hang 2 $
                  "Mode, either ping, tip or query:"
                    <> Pretty.softline
                    <> "ping  - send pings via keep-alive protocol (node-to-node only),"
                    <> Pretty.softline
                    <> "tip   - query tip via chain-sync protocol (node-to-node / node-to-client),"
                    <> Pretty.softline
                    <> "query - query handshake parameters (node-to-node / node-to-client)."
            )
          <> Opt.value Ping.PingMode
          <> Opt.metavar "MODE"
      )
    <*> Opt.option
      Opt.str
      ( Opt.long "srv-prefix"
          <> Opt.help "Prefix that will be added to an SRV service name"
          <> Opt.value "_cardano._tcp"
          <> Opt.metavar "SRV_PREFIX"
          <> Opt.showDefault
      )
    <*> Opt.option
      colorMode
      ( Opt.long "color"
          <> Opt.help "Colorized output: auto, never or always."
          <> Opt.value Ping.ColorAuto
          <> Opt.metavar "COLOR"
          <> Opt.showDefaultWith
            ( \case
                Ping.ColorAuto -> "auto"
                Ping.ColorNever -> "never"
                Ping.ColorAlways -> "always"
            )
      )
    <*> Opt.flag
      Ping.FullHash
      Ping.ShortHash
      ( Opt.long "short-hash"
          <> Opt.help "show short tip's hash"
      )
 where
  pingMode :: Opt.ReadM Ping.PingMode
  pingMode =
    Opt.eitherReader $ \case
      "tip" -> Right Ping.TipMode
      "ping" -> Right Ping.PingMode
      "query" -> Right Ping.QueryMode
      _ -> Left "unexpected string"

  colorMode :: Opt.ReadM Ping.ColorMode
  colorMode =
    Opt.eitherReader $ \case
      "auto" -> Right Ping.ColorAuto
      "never" -> Right Ping.ColorNever
      "always" -> Right Ping.ColorAlways
      _ -> Left "expected auto, never or always"

-- | A copy of @Cardano.Network.Ping.argParser@.
pPingAddresses :: Opt.Parser [Ping.Address (Ping.Unresolved Ping.SRVOrFilePathUnresolved)]
pPingAddresses =
  some pAddress
 where
  pAddress :: Opt.Parser (Ping.Address (Ping.Unresolved Ping.SRVOrFilePathUnresolved))
  pAddress =
    Opt.argument
      ( uncurry Ping.IP <$> readIPv4AndPort
          <|> uncurry Ping.IP <$> readIPv6AndPort
          <|> readDomainNameOrFilePath
      )
      ( Opt.help
          "List of IP/DNS/SRV address and ports or UNIX socket paths, e.g. 127.0.0.1:3001 [::1]:3001 example.org:3001."
          <> Opt.metavar "ADDRS"
      )

  -- note: `Read` instances for `IP`, `IPv4`, `IPv6` expect no trailing
  -- characters after the address, thus we need to find the split position
  -- first.

  -- parse IPv4 address and port in a form `127.0.0.1:3001`
  readIPv4AndPort :: Opt.ReadM (IP, Word)
  readIPv4AndPort =
    Opt.eitherReader $ \s ->
      case splitWith ':' s of
        Nothing -> Left s
        Just (addrStr, portStr) ->
          maybe (Left s) Right $
            (,)
              <$> readMaybe addrStr
              <*> readMaybe portStr

  -- parse IPv6 address and port in a form `[::1]:3001`
  readIPv6AndPort :: Opt.ReadM (IP, Word)
  readIPv6AndPort =
    Opt.eitherReader $ \s ->
      case s of
        ('[' : s') ->
          case splitWith ']' s' of
            Just (addrStr, ':' : portStr) ->
              maybe (Left s) Right $
                (,)
                  <$> readMaybe addrStr
                  <*> readMaybe portStr
            _ -> Left s
        _ -> Left s

  readDomainNameOrFilePath
    :: Opt.ReadM (Ping.Address (Ping.Unresolved Ping.SRVOrFilePathUnresolved))
  readDomainNameOrFilePath = Opt.eitherReader $ Right . Ping.mkAddress

  splitWith :: Char -> String -> Maybe (String, String)
  splitWith c = go ""
   where
    go _ [] = Nothing
    go acc (a : as)
      | a == c = Just (reverse acc, as)
      | otherwise = go (a : acc) as
