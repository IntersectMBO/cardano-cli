{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Pool
  ( PoolCmd(..)
  , renderPoolCmd
  , pPoolCmd
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Orphans ()
import           Cardano.CLI.Types.Key (VerificationKeyOrFile (..))
import           Cardano.CLI.Types.Legacy
import qualified Cardano.Ledger.BaseTypes as Shelley
import           Cardano.Prelude (ConvertText (..))

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import qualified Data.IP as IP
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import           Text.Read (readEither, readMaybe)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

data PoolCmd era
  = PoolRegistrationCert
      (ShelleyBasedEra era)
      -- ^ Era in which to register the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      (File () Out)
  | PoolRetirementCert
      (ShelleyBasedEra era)
      -- ^ Era in which to retire the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      (File () Out)
  | PoolGetId (VerificationKeyOrFile StakePoolKey) PoolIdOutputFormat (Maybe (File () Out))
  | PoolMetadataHash (StakePoolMetadataFile In) (Maybe (File () Out))
  deriving Show

renderPoolCmd :: PoolCmd era -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetadataHash {} -> "stake-pool metadata-hash"

pPoolCmd :: ()
  => EnvCli
  -> CardanoEra era
  -> Parser (PoolCmd era)
pPoolCmd envCli era =
  asum $ catMaybes
    [ pStakePoolRegistrationCertCmd envCli era
    , pStakePoolRetirementCertCmd era
    , pIdCmd
    , pPoolMetadataHashSubCmd
    ]

pPoolMetadataFile :: Parser (StakePoolMetadataFile In)
pPoolMetadataFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "pool-metadata-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the pool metadata."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pEpochNo :: Parser EpochNo
pEpochNo =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "epoch"
    , Opt.metavar "NATURAL"
    , Opt.help "The epoch number."
    ]

pPoolIdOutputFormat :: Parser PoolIdOutputFormat
pPoolIdOutputFormat =
  Opt.option readPoolIdOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional pool id output format. Accepted output formats are \"hex\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value PoolIdOutputFormatBech32
    ]

pMaybeOutputFile :: Parser (Maybe (File content Out))
pMaybeOutputFile =
  optional $ fmap File $ Opt.strOption $ mconcat
    [ Opt.long "out-file"
    , Opt.metavar "FILE"
    , Opt.help "Optional output file. Default is to write to stdout."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pRewardAcctVerificationKeyFile :: Parser (VerificationKeyFile In)
pRewardAcctVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "pool-reward-account-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the reward account stake verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "reward-account-verification-key-file"
      , Opt.internal
      ]
    ]

pRewardAcctVerificationKey :: Parser (VerificationKey StakeKey)
pRewardAcctVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long "pool-reward-account-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Reward account stake verification key (Bech32 or hex-encoded)."
    ]

pRewardAcctVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pRewardAcctVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pRewardAcctVerificationKey
    , VerificationKeyFilePath <$> pRewardAcctVerificationKeyFile
    ]

pPoolOwnerVerificationKeyFile :: Parser (VerificationKeyFile In)
pPoolOwnerVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "pool-owner-stake-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the pool owner stake verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "pool-owner-staking-verification-key"
      , Opt.internal
      ]
    ]

pPoolOwnerVerificationKey :: Parser (VerificationKey StakeKey)
pPoolOwnerVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long "pool-owner-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Pool owner stake verification key (Bech32 or hex-encoded)."
    ]

pPoolOwnerVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pPoolOwnerVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pPoolOwnerVerificationKey
    , VerificationKeyFilePath <$> pPoolOwnerVerificationKeyFile
    ]

pPoolPledge :: Parser Lovelace
pPoolPledge =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "pool-pledge"
    , Opt.metavar "LOVELACE"
    , Opt.help "The stake pool's pledge."
    ]


pPoolCost :: Parser Lovelace
pPoolCost =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "pool-cost"
    , Opt.metavar "LOVELACE"
    , Opt.help "The stake pool's cost."
    ]

pPoolMargin :: Parser Rational
pPoolMargin =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long "pool-margin"
    , Opt.metavar "RATIONAL"
    , Opt.help "The stake pool's margin."
    ]

pPoolRelay :: Parser StakePoolRelay
pPoolRelay =
  asum
    [ pSingleHostAddress
    , pSingleHostName
    , pMultiHostName
    ]

pMultiHostName :: Parser StakePoolRelay
pMultiHostName =
  StakePoolRelayDnsSrvRecord <$> pDNSName
  where
    pDNSName :: Parser ByteString
    pDNSName =
      Opt.option (Opt.eitherReader eDNSName) $ mconcat
        [ Opt.long "multi-host-pool-relay"
        , Opt.metavar "STRING"
        , Opt.help "The stake pool relay's DNS name that corresponds to an SRV DNS record"
        ]

pSingleHostName :: Parser StakePoolRelay
pSingleHostName =
  StakePoolRelayDnsARecord <$> pDNSName <*> optional pPort
  where
    pDNSName :: Parser ByteString
    pDNSName = Opt.option (Opt.eitherReader eDNSName) $ mconcat
      [ Opt.long "single-host-pool-relay"
      , Opt.metavar "STRING"
      , Opt.help $ mconcat
        [ "The stake pool relay's DNS name that corresponds to an"
        , " A or AAAA DNS record"
        ]
      ]

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'Shelley.textToDns' to validate the string.
  case Shelley.textToDns (toS str) of
    Nothing -> Left $ "DNS name is more than 64 bytes: " <> str
    Just dnsName -> Right . Text.encodeUtf8 . Shelley.dnsToText $ dnsName

pSingleHostAddress :: Parser StakePoolRelay
pSingleHostAddress =
  singleHostAddress
    <$> optional pIpV4
    <*> optional pIpV6
    <*> pPort
  where
    singleHostAddress :: Maybe IP.IPv4 -> Maybe IP.IPv6 -> PortNumber -> StakePoolRelay
    singleHostAddress ipv4 ipv6 port =
      case (ipv4, ipv6) of
        (Nothing, Nothing) ->
          error "Please enter either an IPv4 or IPv6 address for the pool relay"
        (Just i4, Nothing) ->
          StakePoolRelayIp (Just i4) Nothing (Just port)
        (Nothing, Just i6) ->
          StakePoolRelayIp Nothing (Just i6) (Just port)
        (Just i4, Just i6) ->
          StakePoolRelayIp (Just i4) (Just i6) (Just port)


pIpV4 :: Parser IP.IPv4
pIpV4 =
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv4) $ mconcat
    [ Opt.long "pool-relay-ipv4"
    , Opt.metavar "STRING"
    , Opt.help "The stake pool relay's IPv4 address"
    ]

pIpV6 :: Parser IP.IPv6
pIpV6 =
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv6) $ mconcat
    [ Opt.long "pool-relay-ipv6"
    , Opt.metavar "STRING"
    , Opt.help "The stake pool relay's IPv6 address"
    ]

pPort :: Parser PortNumber
pPort =
  Opt.option (fromInteger <$> Opt.eitherReader readEither) $ mconcat
    [ Opt.long "pool-relay-port"
    , Opt.metavar "INT"
    , Opt.help "The stake pool relay's port"
    ]

pStakePoolMetadataReference :: Parser (Maybe StakePoolMetadataReference)
pStakePoolMetadataReference =
  optional $
    StakePoolMetadataReference
      <$> pStakePoolMetadataUrl
      <*> pStakePoolMetadataHash

pStakePoolMetadataUrl :: Parser Text
pStakePoolMetadataUrl =
  Opt.option (readURIOfMaxLength 64) $ mconcat
    [ Opt.long "metadata-url"
    , Opt.metavar "URL"
    , Opt.help "Pool metadata URL (maximum length of 64 characters)."
    ]

pStakePoolMetadataHash :: Parser (Hash StakePoolMetadata)
pStakePoolMetadataHash =
  Opt.option (Opt.eitherReader metadataHash) $ mconcat
    [ Opt.long "metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Pool metadata hash."
    ]
  where
    metadataHash :: String -> Either String (Hash StakePoolMetadata)
    metadataHash =
      first displayError
        . deserialiseFromRawBytesHex (AsHash AsStakePoolMetadata)
        . BSC.pack

pStakePoolRegistrationCertCmd :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (PoolCmd era))
pStakePoolRegistrationCertCmd envCli =
  featureInEra Nothing $ \w -> Just
    $ subParser "registration-certificate"
    $ Opt.info (pCmd w)
    $ Opt.progDesc "Create a stake pool registration certificate"
  where
    pCmd era =
      PoolRegistrationCert era
        <$> pStakePoolVerificationKeyOrFile
        <*> pVrfVerificationKeyOrFile
        <*> pPoolPledge
        <*> pPoolCost
        <*> pPoolMargin
        <*> pRewardAcctVerificationKeyOrFile
        <*> some pPoolOwnerVerificationKeyOrFile
        <*> many pPoolRelay
        <*> pStakePoolMetadataReference
        <*> pNetworkId envCli
        <*> pOutputFile

pStakePoolRetirementCertCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (PoolCmd era))
pStakePoolRetirementCertCmd =
  featureInEra Nothing $ \w -> Just
    $ subParser "deregistration-certificate"
    $ Opt.info (pCmd w)
    $ Opt.progDesc "Create a stake pool deregistration certificate"
  where
    pCmd era =
      PoolRetirementCert era
        <$> pStakePoolVerificationKeyOrFile
        <*> pEpochNo
        <*> pOutputFile

pIdCmd :: Maybe (Parser (PoolCmd era))
pIdCmd =
  Just
    $ subParser "id"
    $ Opt.info pCmd
    $ Opt.progDesc "Build pool id from the offline key"
  where
    pCmd =
      PoolGetId
        <$> pStakePoolVerificationKeyOrFile
        <*> pPoolIdOutputFormat
        <*> pMaybeOutputFile

pPoolMetadataHashSubCmd :: Maybe (Parser (PoolCmd era))
pPoolMetadataHashSubCmd =
  Just
    $ subParser "metadata-hash"
    $ Opt.info pCmd
    $ Opt.progDesc "Print the hash of pool metadata."
  where
    pCmd =
      PoolMetadataHash
        <$> pPoolMetadataFile
        <*> pMaybeOutputFile
