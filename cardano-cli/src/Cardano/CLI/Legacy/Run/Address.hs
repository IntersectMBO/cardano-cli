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

import           Cardano.CLI.EraBased.Run.Address
import           Cardano.CLI.EraBased.Run.Address.Info
import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.AddressInfoError
import           Cardano.CLI.Types.Key (PaymentVerifier (..), StakeIdentifier (..),
                   VerificationKeyTextOrFile)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Function
import           Data.Text (Text)

runLegacyAddressCmds :: LegacyAddressCmds -> ExceptT AddressCmdError IO ()
runLegacyAddressCmds = \case
  AddressKeyGen fmt kt vkf skf ->
    runLegacyAddressKeyGenCmd fmt kt vkf skf
  AddressKeyHash vkf mOFp ->
    runLegacyAddressKeyHashCmd vkf mOFp
  AddressBuild paymentVerifier mbStakeVerifier nw mOutFp ->
    runLegacyAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp
  AddressInfo txt mOFp ->
    runLegacyAddressInfoCmd txt mOFp & firstExceptT AddressCmdAddressInfoError

runLegacyAddressKeyGenCmd :: ()
  => KeyOutputFormat
  -> AddressKeyType
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
runLegacyAddressKeyGenCmd = runAddressKeyGenCmd

runLegacyAddressKeyHashCmd :: ()
  => VerificationKeyTextOrFile
  -> Maybe (File () Out)
  -> ExceptT AddressCmdError IO ()
runLegacyAddressKeyHashCmd = runAddressKeyHashCmd

runLegacyAddressBuildCmd :: ()
  => PaymentVerifier
  -> Maybe StakeIdentifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT AddressCmdError IO ()
runLegacyAddressBuildCmd = runAddressBuildCmd

runLegacyAddressInfoCmd :: ()
  => Text
  -> Maybe (File () Out)
  -> ExceptT AddressInfoError IO ()
runLegacyAddressInfoCmd = runAddressInfoCmd
