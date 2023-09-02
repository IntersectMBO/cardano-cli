{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Run.Address
  ( runLegacyAddressCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Run.Address as EraBased
import qualified Cardano.CLI.EraBased.Run.Address.Info as EraBased
import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyAddressCmdError
import           Cardano.CLI.Types.Errors.ShelleyAddressInfoError
import           Cardano.CLI.Types.Key (PaymentVerifier (..), StakeIdentifier (..),
                   VerificationKeyTextOrFile)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Function
import           Data.Text (Text)

runLegacyAddressCmds :: LegacyAddressCmds -> ExceptT ShelleyAddressCmdError IO ()
runLegacyAddressCmds = \case
  AddressKeyGen fmt kt vkf skf ->
    runLegacyAddressKeyGenCmd fmt kt vkf skf
  AddressKeyHash vkf mOFp ->
    runLegacyAddressKeyHashCmd vkf mOFp
  AddressBuild paymentVerifier mbStakeVerifier nw mOutFp ->
    runLegacyAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp
  AddressInfo txt mOFp ->
    runLegacyAddressInfoCmd txt mOFp & firstExceptT ShelleyAddressCmdAddressInfoError

runLegacyAddressKeyGenCmd :: ()
  => KeyOutputFormat
  -> AddressKeyType
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT ShelleyAddressCmdError IO ()
runLegacyAddressKeyGenCmd = EraBased.runLegacyAddressKeyGenCmd

runLegacyAddressKeyHashCmd :: ()
  => VerificationKeyTextOrFile
  -> Maybe (File () Out)
  -> ExceptT ShelleyAddressCmdError IO ()
runLegacyAddressKeyHashCmd = EraBased.runLegacyAddressKeyHashCmd

runLegacyAddressBuildCmd :: ()
  => PaymentVerifier
  -> Maybe StakeIdentifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyAddressCmdError IO ()
runLegacyAddressBuildCmd = EraBased.runLegacyAddressBuildCmd

runLegacyAddressInfoCmd :: ()
  => Text
  -> Maybe (File () Out)
  -> ExceptT ShelleyAddressInfoError IO ()
runLegacyAddressInfoCmd = EraBased.runLegacyAddressInfoCmd
