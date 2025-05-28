{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Genesis.Internal.Common
  ( decodeShelleyGenesisFile
  , decodeAlonzoGenesisFile
  , decodeConwayGenesisFile
  , genStuffedAddress
  , getCurrentTimePlus30
  , readRelays

    -- * Protocol Parameters
  , readProtocolParameters
  )
where

import Cardano.Api hiding (ConwayEra)
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (AlonzoGenesis, ConwayGenesis)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (ShelleyGenesis, ShelleyLedgerEra, decodeAlonzoGenesis)

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GenesisCmdError
import Cardano.CLI.Type.Error.ProtocolParamsError
import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Random qualified as Crypto

import Data.Aeson qualified as A
import Data.Binary.Get qualified as Bin
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Map.Strict (Map)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)

import Crypto.Random (getRandomBytes)

decodeShelleyGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => FilePath
  -> t m ShelleyGenesis
decodeShelleyGenesisFile = readAndDecodeGenesisFileWith A.eitherDecode

-- | Decode Alonzo Genesis file. See 'Cardano.Api.Genesis.decodeAlonzoGenesis' haddocks for details.
decodeAlonzoGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => Maybe (CardanoEra era)
  -- ^ Optional era in which we're decoding alonzo genesis.
  -> FilePath
  -> t m AlonzoGenesis
decodeAlonzoGenesisFile mEra = readAndDecodeGenesisFileWith (runExcept . decodeAlonzoGenesis mEra)

decodeConwayGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => FilePath
  -> t m ConwayGenesis
decodeConwayGenesisFile = readAndDecodeGenesisFileWith A.eitherDecode

readAndDecodeGenesisFileWith
  :: MonadIOTransError GenesisCmdError t m
  => (LBS.ByteString -> Either String a)
  -> FilePath
  -> t m a
readAndDecodeGenesisFileWith decode' fpath = do
  lbs <-
    handleIOExceptionsLiftWith (GenesisCmdGenesisFileError . FileIOError fpath) . liftIO $
      LBS.readFile fpath
  modifyError (GenesisCmdGenesisFileDecodeError fpath . Text.pack)
    . hoistEither
    $ decode' lbs

genStuffedAddress :: L.Network -> IO (AddressInEra ShelleyEra)
genStuffedAddress network = do
  paymentCredential <-
    L.KeyHashObj . mkKeyHash . read64BitInt <$> Crypto.runSecureRandom (getRandomBytes 8)
  pure . shelleyAddressInEra ShelleyBasedEraShelley $
    ShelleyAddress network paymentCredential L.StakeRefNull
 where
  read64BitInt :: ByteString -> Int
  read64BitInt =
    (fromIntegral :: Word64 -> Int)
      . Bin.runGet Bin.getWord64le
      . LBS.fromStrict

  mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
  mkDummyHash _ = coerce . L.hashWithSerialiser @h L.toCBOR

  mkKeyHash :: forall discriminator. Int -> L.KeyHash discriminator
  mkKeyHash = L.KeyHash . mkDummyHash (Proxy @L.ADDRHASH)

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: MonadIO m => m UTCTime
getCurrentTimePlus30 =
  plus30sec <$> liftIO getCurrentTime
 where
  plus30sec :: UTCTime -> UTCTime
  plus30sec = addUTCTime (30 :: NominalDiffTime)

-- @readRelays fp@ reads the relays specification from a file
readRelays
  :: ()
  => MonadIO m
  => FilePath
  -- ^ The file to read from
  -> ExceptT GenesisCmdError m (Map Word [L.StakePoolRelay])
readRelays fp = do
  relaySpecJsonBs <-
    handleIOExceptT (GenesisCmdStakePoolRelayFileError fp) (LBS.readFile fp)
  firstExceptT (GenesisCmdStakePoolRelayJsonDecodeError fp)
    . hoistEither
    $ A.eitherDecode relaySpecJsonBs

-- TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters
  :: forall era
   . Exp.IsEra era
  => ProtocolParamsFile
  -> ExceptT ProtocolParamsError IO (L.PParams (ShelleyLedgerEra era))
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ProtocolParamsErrorFile . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ProtocolParamsErrorJSON fpath . Text.pack) . hoistEither $
    obtainCommonConstraints (Exp.useEra @era) $
      A.eitherDecode' pparams
