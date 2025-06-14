{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Byron.Parser
  ( ByronCommand (..)
  , NodeCmds (..)
  , backwardsCompatibilityCommands
  , parseByronCommands
  , parseHeavyDelThd
  , parseInstallerHash
  , parseMaxBlockSize
  , parseMaxHeaderSize
  , parseMaxTxSize
  , parseMaxProposalSize
  , parseMpcThd
  , parseScriptVersion
  , parseSlotDuration
  , parseSoftforkRule
  , parseSystemTag
  , parseTxFeePolicy
  , parseUpdateProposalThd
  , parseUpdateProposalTTL
  , parseUnlockStakeEpoch
  , parseUpdateVoteThd
  )
where

import Cardano.Api hiding (GenesisParameters, UpdateProposal, parseTxIn)
import Cardano.Api.Byron (ByronProtocolParametersUpdate (..))
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Byron.Command
import Cardano.CLI.Byron.Genesis
import Cardano.CLI.Byron.Key
import Cardano.CLI.Byron.Tx
import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option hiding (parseLovelace)
import Cardano.CLI.Parser (commandWithMetavar)
import Cardano.CLI.Run (ClientCommand (ByronCommand))
import Cardano.CLI.Type.Common
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Crypto.Hashing (hashRaw)
import Cardano.Crypto.ProtocolMagic
  ( AProtocolMagic (..)
  , ProtocolMagic
  , ProtocolMagicId (..)
  )

import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Attoparsec.Combinator ((<?>))
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Char qualified as Char
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word16, Word64)
import Formatting (build, sformat)
import GHC.Natural (Natural)
import GHC.Word (Word8)
import Options.Applicative
import Options.Applicative qualified as Opt

backwardsCompatibilityCommands :: EnvCli -> Parser ClientCommand
backwardsCompatibilityCommands envCli =
  asum hiddenCmds
 where
  convertToByronCommand :: Mod CommandFields ByronCommand -> Parser ClientCommand
  convertToByronCommand p = ByronCommand <$> Opt.subparser (p <> Opt.internal)

  hiddenCmds :: [Parser ClientCommand]
  hiddenCmds =
    concatMap
      (fmap convertToByronCommand)
      [ parseGenesisRelatedValues
      , parseKeyRelatedValues envCli
      , parseTxRelatedValues envCli
      , parseMiscellaneous
      ]

-- Implemented with asum so all commands don't get hidden when trying to hide
-- the 'pNodeCmdBackwardCompatible' parser.
parseByronCommands :: EnvCli -> Parser ByronCommand
parseByronCommands envCli =
  asum
    [ subParser'
        "key"
        ( Opt.info (asum (map Opt.subparser (parseKeyRelatedValues envCli)) <**> Opt.helper) $
            Opt.progDesc "Byron key utility commands"
        )
    , subParser'
        "transaction"
        ( Opt.info (asum (map Opt.subparser (parseTxRelatedValues envCli)) <**> Opt.helper) $
            Opt.progDesc "Byron transaction commands"
        )
    , subParser'
        "genesis"
        ( Opt.info (asum (map Opt.subparser parseGenesisRelatedValues) <**> Opt.helper) $
            Opt.progDesc "Byron genesis block commands"
        )
    , subParser'
        "governance"
        ( Opt.info ((NodeCmds <$> Opt.subparser (pNodeCmds envCli)) <**> Opt.helper) $
            Opt.progDesc "Byron governance commands"
        )
    , subParser'
        "miscellaneous"
        ( Opt.info (asum (map Opt.subparser parseMiscellaneous) <**> Opt.helper) $
            Opt.progDesc "Byron miscellaneous commands"
        )
    , NodeCmds <$> pNodeCmdBackwardCompatible envCli
    ]
 where
  subParser' :: String -> ParserInfo ByronCommand -> Parser ByronCommand
  subParser' name pInfo = Opt.subparser $ commandWithMetavar name pInfo

pNodeCmdBackwardCompatible :: EnvCli -> Parser NodeCmds
pNodeCmdBackwardCompatible envCli = Opt.subparser $ pNodeCmds envCli <> Opt.internal

parseCBORObject :: Parser CBORObject
parseCBORObject =
  asum
    [ CBORBlockByron
        <$> Opt.option
          auto
          ( long "byron-block"
              <> help
                ( "The CBOR file is a byron era block."
                    <> " Enter the number of slots in an epoch. The default value is 21600"
                )
              <> metavar "INT"
              <> value (EpochSlots 21600)
          )
    , flag' CBORDelegationCertificateByron $
        long "byron-delegation-certificate"
          <> help "The CBOR file is a byron era delegation certificate"
    , flag' CBORTxByron $
        long "byron-tx"
          <> help "The CBOR file is a byron era tx"
    , flag' CBORUpdateProposalByron $
        long "byron-update-proposal"
          <> help "The CBOR file is a byron era update proposal"
    , flag' CBORVoteByron $
        long "byron-vote"
          <> help "The CBOR file is a byron era vote"
    ]

-- | Values required to create genesis.
parseGenesisParameters :: Parser GenesisParameters
parseGenesisParameters =
  GenesisParameters
    <$> parseUTCTime
      "start-time"
      "Start time of the new cluster to be enshrined in the new genesis."
    <*> parseFilePath
      "protocol-parameters-file"
      "JSON file with protocol parameters."
    <*> parseK
    <*> parseProtocolMagic
    <*> parseTestnetBalanceOptions
    <*> parseFakeAvvmOptions
    <*> ( Byron.rationalToLovelacePortion
            <$> parseFractionWithDefault
              "avvm-balance-factor"
              "AVVM balances will be multiplied by this factor (defaults to 1)."
              1
        )
    <*> optional
      ( parseIntegral
          "secret-seed"
          "Optionally specify the seed of generation."
      )

parseGenesisRelatedValues :: [Mod CommandFields ByronCommand]
parseGenesisRelatedValues =
  [ command' "genesis" "Create genesis." $
      Genesis
        <$> parseNewDirectory
          "genesis-output-dir"
          "Non-existent directory where genesis JSON file and secrets shall be placed."
        <*> parseGenesisParameters
  , command' "print-genesis-hash" "Compute hash of a genesis file." $
      PrintGenesisHash
        <$> parseGenesisFile "genesis-json"
  ]

-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: EnvCli -> [Mod CommandFields ByronCommand]
parseKeyRelatedValues envCli =
  [ command' "keygen" "Generate a signing key." $
      Keygen
        <$> parseNewSigningKeyFile "secret"
  , command'
      "to-verification"
      "Extract a verification key in its base64 form."
      $ ToVerification
        <$> parseByronKeyFormat
        <*> parseSigningKeyFile
          "secret"
          "Signing key file to extract the verification part from."
        <*> parseNewVerificationKeyFile "to"
  , command'
      "signing-key-public"
      "Pretty-print a signing key's verification key (not a secret)."
      $ PrettySigningKeyPublic
        <$> parseByronKeyFormat
        <*> parseSigningKeyFile
          "secret"
          "Signing key to pretty-print."
  , command'
      "signing-key-address"
      "Print address of a signing key."
      $ PrintSigningKeyAddress
        <$> parseByronKeyFormat
        <*> pNetworkId envCli
        <*> parseSigningKeyFile
          "secret"
          "Signing key, whose address is to be printed."
  , command'
      "migrate-delegate-key-from"
      "Migrate a delegate key from an older version."
      $ MigrateDelegateKeyFrom
        <$> parseSigningKeyFile "from" "Legacy signing key file to migrate."
        <*> parseNewSigningKeyFile "to"
  ]

parseMiscellaneous :: [Mod CommandFields ByronCommand]
parseMiscellaneous =
  [ command'
      "validate-cbor"
      "Validate a CBOR blockchain object."
      $ ValidateCBOR
        <$> parseCBORObject
        <*> parseFilePath "filepath" "Filepath of CBOR file."
  , command'
      "pretty-print-cbor"
      "Pretty print a CBOR file."
      $ PrettyPrintCBOR
        <$> parseFilePath "filepath" "Filepath of CBOR file."
  ]

parseTestnetBalanceOptions :: Parser Byron.TestnetBalanceOptions
parseTestnetBalanceOptions =
  Byron.TestnetBalanceOptions
    <$> parseIntegral
      "n-poor-addresses"
      "Number of poor nodes (with small balance)."
    <*> parseIntegral
      "n-delegate-addresses"
      "Number of delegate nodes (with huge balance)."
    <*> parseLovelace
      "total-balance"
      "Total balance owned by these nodes."
    <*> parseFraction
      "delegate-share"
      "Portion of stake owned by all delegates together."

parseTxIn :: Parser TxIn
parseTxIn =
  Opt.option
    (readerFromAttoParser parseTxInAtto)
    $ long "txin"
      <> metavar "(TXID,INDEX)"
      <> help "Transaction input is a pair of an UTxO TxId and a zero-based output index."

parseTxInAtto :: Atto.Parser TxIn
parseTxInAtto =
  TxIn
    <$> (Atto.char '(' *> parseTxIdAtto <* Atto.char ',')
    <*> (parseTxIxAtto <* Atto.char ')')

parseTxIdAtto :: Atto.Parser TxId
parseTxIdAtto = (<?> "Transaction ID (hexadecimal)") $ do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex bstr of
    Right addr -> return addr
    Left e -> fail $ docToString $ "Incorrect transaction id format: " <> prettyError e

parseTxIxAtto :: Atto.Parser TxIx
parseTxIxAtto = toEnum <$> Atto.decimal

parseTxOut :: Parser (TxOut CtxTx ByronEra)
parseTxOut =
  Opt.option
    ( ( \(addr, lovelace) ->
          TxOut
            (pAddressInEra addr)
            (pLovelaceTxOut lovelace)
            TxOutDatumNone
            ReferenceScriptNone
      )
        <$> auto
    )
    $ long "txout"
      <> metavar "'(\"ADDR\", LOVELACE)'"
      <> help "Specify a transaction output, as a pair of an address and lovelace."
 where
  pAddressInEra :: Text -> AddressInEra ByronEra
  pAddressInEra t =
    case Byron.decodeAddressBase58 t of
      Left err -> error $ "Bad Base58 address: " <> show err
      Right byronAddress -> AddressInEra ByronAddressInAnyEra $ ByronAddress byronAddress

  pLovelaceTxOut :: Word64 -> TxOutValue ByronEra
  pLovelaceTxOut l =
    if l > (maxBound :: Word64)
      then error $ show l <> " lovelace exceeds the Word64 upper bound"
      else TxOutValueByron $ L.Coin $ toInteger l

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

parseTxRelatedValues :: EnvCli -> [Mod CommandFields ByronCommand]
parseTxRelatedValues envCli =
  [ command'
      "submit-tx"
      "Submit a raw, signed transaction, in its on-wire representation."
      $ SubmitTx
        <$> pSocketPath envCli
        <*> pNetworkId envCli
        <*> parseTxFile "tx"
  , command'
      "issue-genesis-utxo-expenditure"
      "Write a file with a signed transaction, spending genesis UTxO."
      $ SpendGenesisUTxO
        <$> parseGenesisFile "genesis-json"
        <*> pNetworkId envCli
        <*> parseByronKeyFormat
        <*> parseNewTxFile "tx"
        <*> parseSigningKeyFile
          "wallet-key"
          "Key that has access to all mentioned genesis UTxO inputs."
        <*> parseAddress
          "rich-addr-from"
          "Tx source: genesis UTxO richman address (non-HD)."
        <*> some parseTxOut
  , command'
      "issue-utxo-expenditure"
      "Write a file with a signed transaction, spending normal UTxO."
      $ SpendUTxO
        <$> pNetworkId envCli
        <*> parseByronKeyFormat
        <*> parseNewTxFile "tx"
        <*> parseSigningKeyFile
          "wallet-key"
          "Key that has access to all mentioned genesis UTxO inputs."
        <*> some parseTxIn
        <*> some parseTxOut
  , command'
      "txid"
      "Print the txid of a raw, signed transaction."
      $ GetTxId
        <$> parseTxFile "tx"
  ]

pNodeCmds :: EnvCli -> Mod CommandFields NodeCmds
pNodeCmds envCli =
  mconcat
    [ Opt.command "create-update-proposal" $
        Opt.info (parseByronUpdateProposal envCli <**> Opt.helper) $
          Opt.progDesc "Create an update proposal."
    , Opt.command "create-proposal-vote" $
        Opt.info (parseByronVote envCli <**> Opt.helper) $
          Opt.progDesc "Create an update proposal vote."
    , Opt.command "submit-update-proposal" $
        Opt.info (parseByronUpdateProposalSubmission envCli <**> Opt.helper) $
          Opt.progDesc "Submit an update proposal."
    , Opt.command "submit-proposal-vote" $
        Opt.info (parseByronVoteSubmission envCli <**> Opt.helper) $
          Opt.progDesc "Submit a proposal vote."
    ]

parseByronUpdateProposal :: EnvCli -> Parser NodeCmds
parseByronUpdateProposal envCli = do
  UpdateProposal
    <$> pNetworkId envCli
    <*> parseSigningKeyFile "signing-key" "Path to signing key."
    <*> parseProtocolVersion
    <*> parseSoftwareVersion
    <*> parseSystemTag
    <*> parseInstallerHash
    <*> parseFilePath "filepath" "Byron proposal output filepath."
    <*> pByronProtocolParametersUpdate

parseByronVoteSubmission :: EnvCli -> Parser NodeCmds
parseByronVoteSubmission envCli = do
  SubmitVote
    <$> pSocketPath envCli
    <*> pNetworkId envCli
    <*> parseFilePath "filepath" "Filepath of Byron update proposal vote."

pByronProtocolParametersUpdate :: Parser ByronProtocolParametersUpdate
pByronProtocolParametersUpdate =
  ByronProtocolParametersUpdate
    <$> optional parseScriptVersion
    <*> optional parseSlotDuration
    <*> optional parseMaxBlockSize
    <*> optional parseMaxHeaderSize
    <*> optional parseMaxTxSize
    <*> optional parseMaxProposalSize
    <*> optional parseMpcThd
    <*> optional parseHeavyDelThd
    <*> optional parseUpdateVoteThd
    <*> optional parseUpdateProposalThd
    <*> optional parseUpdateProposalTTL
    <*> optional parseSoftforkRule
    <*> optional parseTxFeePolicy
    <*> optional parseUnlockStakeEpoch

parseByronUpdateProposalSubmission :: EnvCli -> Parser NodeCmds
parseByronUpdateProposalSubmission envCli =
  SubmitUpdateProposal
    <$> pSocketPath envCli
    <*> pNetworkId envCli
    <*> parseFilePath "filepath" "Filepath of Byron update proposal."

parseByronVote :: EnvCli -> Parser NodeCmds
parseByronVote envCli =
  CreateVote
    <$> pNetworkId envCli
    <*> (File <$> parseFilePath "signing-key" "Filepath of signing key.")
    <*> parseFilePath "proposal-filepath" "Filepath of Byron update proposal."
    <*> parseVoteBool
    <*> parseFilePath "output-filepath" "Byron vote output filepath."

--------------------------------------------------------------------------------
-- CLI Parsers
--------------------------------------------------------------------------------

parseScriptVersion :: Parser Word16
parseScriptVersion =
  Opt.option
    auto
    ( long "script-version"
        <> metavar "WORD16"
        <> help "Proposed script version."
    )

parseSlotDuration :: Parser Natural
parseSlotDuration =
  Opt.option
    auto
    ( long "slot-duration"
        <> metavar "NATURAL"
        <> help "Proposed slot duration."
    )

parseSystemTag :: Parser Byron.SystemTag
parseSystemTag =
  Opt.option
    (eitherReader checkSysTag)
    ( long "system-tag"
        <> metavar "STRING"
        <> help "Identify which system (linux, win64, etc) the update proposal is for."
    )
 where
  checkSysTag :: String -> Either String Byron.SystemTag
  checkSysTag name =
    let tag = Byron.SystemTag $ Text.pack name
     in case Byron.checkSystemTag tag of
          Left err -> Left . Text.unpack $ sformat build err
          Right () -> Right tag

parseInstallerHash :: Parser Byron.InstallerHash
parseInstallerHash =
  Byron.InstallerHash . hashRaw . C8.pack
    <$> strOption
      ( long "installer-hash"
          <> metavar "HASH"
          <> help "Software hash."
      )

parseMaxBlockSize :: Parser Natural
parseMaxBlockSize =
  Opt.option
    auto
    ( long "max-block-size"
        <> metavar "NATURAL"
        <> help "Proposed max block size."
    )

parseMaxHeaderSize :: Parser Natural
parseMaxHeaderSize =
  Opt.option
    auto
    ( long "max-header-size"
        <> metavar "NATURAL"
        <> help "Proposed max block header size."
    )

parseMaxTxSize :: Parser Natural
parseMaxTxSize =
  Opt.option
    auto
    ( long "max-tx-size"
        <> metavar "NATURAL"
        <> help "Proposed max transaction size."
    )

parseMaxProposalSize :: Parser Natural
parseMaxProposalSize =
  Opt.option
    auto
    ( long "max-proposal-size"
        <> metavar "NATURAL"
        <> help "Proposed max update proposal size."
    )

parseMpcThd :: Parser Byron.LovelacePortion
parseMpcThd =
  Byron.rationalToLovelacePortion
    <$> parseFraction "max-mpc-thd" "Proposed max mpc threshold."

parseProtocolVersion :: Parser Byron.ProtocolVersion
parseProtocolVersion =
  Byron.ProtocolVersion
    <$> (parseWord "protocol-version-major" "Protocol version major." "WORD16" :: Parser Word16)
    <*> (parseWord "protocol-version-minor" "Protocol version minor." "WORD16" :: Parser Word16)
    <*> (parseWord "protocol-version-alt" "Protocol version alt." "WORD8" :: Parser Word8)

parseHeavyDelThd :: Parser Byron.LovelacePortion
parseHeavyDelThd =
  Byron.rationalToLovelacePortion
    <$> parseFraction "heavy-del-thd" "Proposed heavy delegation threshold."

parseUpdateVoteThd :: Parser Byron.LovelacePortion
parseUpdateVoteThd =
  Byron.rationalToLovelacePortion
    <$> parseFraction "update-vote-thd" "Propose update vote threshold."

parseUpdateProposalThd :: Parser Byron.LovelacePortion
parseUpdateProposalThd =
  Byron.rationalToLovelacePortion
    <$> parseFraction "update-proposal-thd" "Propose update proposal threshold."

parseUpdateProposalTTL :: Parser Byron.SlotNumber
parseUpdateProposalTTL =
  Byron.SlotNumber
    <$> Opt.option
      auto
      ( long "time-to-live"
          <> metavar "WORD64"
          <> help "Proposed time for an update proposal to live."
      )

parseSoftforkRule :: Parser Byron.SoftforkRule
parseSoftforkRule =
  ( Byron.SoftforkRule . Byron.rationalToLovelacePortion
      <$> parseFraction
        "softfork-init-thd"
        "Propose initial threshold (right after proposal is confirmed)."
  )
    <*> ( Byron.rationalToLovelacePortion
            <$> parseFraction "softfork-min-thd" "Propose minimum threshold (threshold can't be less than this)."
        )
    <*> ( Byron.rationalToLovelacePortion
            <$> parseFraction
              "softfork-thd-dec"
              "Propose threshold decrement (threshold will decrease by this amount after each epoch)."
        )

parseSoftwareVersion :: Parser Byron.SoftwareVersion
parseSoftwareVersion =
  Byron.SoftwareVersion <$> parseApplicationName <*> parseNumSoftwareVersion

parseApplicationName :: Parser Byron.ApplicationName
parseApplicationName =
  Opt.option
    (eitherReader checkAppNameLength)
    ( long "application-name"
        <> metavar "STRING"
        <> help "The name of the application."
    )
 where
  checkAppNameLength :: String -> Either String Byron.ApplicationName
  checkAppNameLength name =
    let appName = Byron.ApplicationName $ Text.pack name
     in case Byron.checkApplicationName appName of
          Left err -> Left . Text.unpack $ sformat build err
          Right () -> Right appName

parseNumSoftwareVersion :: Parser Byron.NumSoftwareVersion
parseNumSoftwareVersion =
  parseWord
    "software-version-num"
    "Numeric software version associated with application name."
    "WORD32"

parseTxFeePolicy :: Parser Byron.TxFeePolicy
parseTxFeePolicy =
  Byron.TxFeePolicyTxSizeLinear
    <$> ( Byron.TxSizeLinear
            <$> parseLovelace "tx-fee-a-constant" "Propose the constant a for txfee = a + b*s where s is the size."
            <*> parseFraction "tx-fee-b-constant" "Propose the constant b for txfee = a + b*s where s is the size."
        )

parseVoteBool :: Parser Bool
parseVoteBool =
  flag' True (long "vote-yes" <> help "Vote yes with respect to an update proposal.")
    <|> flag' False (long "vote-no" <> help "Vote no with respect to an update proposal.")

parseUnlockStakeEpoch :: Parser Byron.EpochNumber
parseUnlockStakeEpoch =
  Byron.EpochNumber
    <$> Opt.option
      auto
      ( long "unlock-stake-epoch"
          <> metavar "WORD64"
          <> help "Proposed epoch to unlock all stake."
      )

parseWord :: Integral a => String -> String -> String -> Parser a
parseWord optname desc metvar =
  Opt.option (fromInteger <$> auto) $
    long optname <> metavar metvar <> help desc

parseAddress :: String -> String -> Parser (Address ByronAddr)
parseAddress opt desc =
  Opt.option (cliParseBase58Address <$> str) $
    long opt <> metavar "ADDR" <> help desc

parseByronKeyFormat :: Parser ByronKeyFormat
parseByronKeyFormat =
  asum
    [ flag' LegacyByronKeyFormat $
        long "byron-legacy-formats"
          <> help "Byron/cardano-sl formats and compatibility"
    , flag' NonLegacyByronKeyFormat $
        long "byron-formats"
          <> help "Byron era formats and compatibility"
    , -- And hidden compatibility flag aliases that should be deprecated:
      flag' LegacyByronKeyFormat $ hidden <> long "byron-legacy"
    , flag' NonLegacyByronKeyFormat $ hidden <> long "real-pbft"
    , -- Default Byron key format
      pure NonLegacyByronKeyFormat
    ]

parseFakeAvvmOptions :: Parser Byron.FakeAvvmOptions
parseFakeAvvmOptions =
  Byron.FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseLovelace "avvm-entry-balance" "AVVM address."

parseK :: Parser Byron.BlockCount
parseK =
  Byron.BlockCount
    <$> parseIntegral "k" "The security parameter of the Ouroboros protocol."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

parseFractionWithDefault
  :: String
  -> String
  -> Double
  -> Parser Rational
parseFractionWithDefault optname desc w =
  toRational
    <$> Opt.option
      readDouble
      ( long optname
          <> metavar "DOUBLE"
          <> help desc
          <> value w
      )

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  mkProtocol <$> parseProtocolMagicId "protocol-magic"
 where
  mkProtocol protMagicId = AProtocolMagic (L.Annotated protMagicId ()) RequiresMagic

parseTxFile :: String -> Parser (TxFile In)
parseTxFile opt =
  File
    <$> parseFilePath opt "File containing the signed transaction."

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  Opt.option (posixSecondsToUTCTime . fromInteger <$> auto) $
    long optname <> metavar "POSIXSECONDS" <> help desc

cliParseBase58Address :: Text -> Address ByronAddr
cliParseBase58Address t =
  case Byron.decodeAddressBase58 t of
    Left err -> error $ "Bad Base58 address: " <> show err
    Right byronAddress -> ByronAddress byronAddress

parseFraction :: String -> String -> Parser Rational
parseFraction optname desc =
  Opt.option (toRational <$> readDouble) $
    long optname
      <> metavar "DOUBLE"
      <> help desc

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc =
  Opt.option (fromInteger <$> auto) $
    long optname <> metavar "INT" <> help desc

parseLovelace :: String -> String -> Parser Byron.Lovelace
parseLovelace optname desc =
  Opt.option
    (readerFromAttoParser parseLovelaceAtto)
    ( long optname
        <> metavar "INT"
        <> help desc
    )
 where
  parseLovelaceAtto :: Atto.Parser Byron.Lovelace
  parseLovelaceAtto = do
    i <- Atto.decimal
    if i > toInteger (maxBound :: Word64)
      then fail $ show i <> " lovelace exceeds the Word64 upper bound"
      else case toByronLovelace $ L.Coin i of
        Just byronLovelace -> return byronLovelace
        Nothing -> error $ "Error converting lovelace: " <> show i

readDouble :: ReadM Double
readDouble = do
  f <- auto
  when (f < 0) $ readerError "fraction must be >= 0"
  when (f > 1) $ readerError "fraction must be <= 1"
  return f

parseSigningKeyFile :: String -> String -> Parser (SigningKeyFile In)
parseSigningKeyFile opt desc = File <$> parseFilePath opt desc

parseGenesisFile :: String -> Parser GenesisFile
parseGenesisFile opt =
  GenesisFile <$> parseFilePath opt "Genesis JSON file."
