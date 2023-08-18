{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Types.Errors.StakeAddress
  ( ShelleyStakeAddressCmdError(..)
  , StakeAddressRegistrationError(..)
  , StakeAddressDelegationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read

import qualified Data.Text as Text

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  | StakeDelegationError !StakeAddressDelegationError
  deriving Show


instance Error ShelleyStakeAddressCmdError where
  displayError = \case
    ShelleyStakeAddressCmdReadKeyFileError fileErr    -> displayError fileErr
    ShelleyStakeAddressCmdWriteFileError fileErr      -> displayError fileErr
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> displayError fileErr
    StakeRegistrationError regErr                     -> displayError regErr
    StakeDelegationError delegErr                     -> displayError delegErr

data StakeAddressRegistrationError = StakeAddressRegistrationDepositRequired deriving Show

instance Error StakeAddressRegistrationError where
  displayError = \case
    StakeAddressRegistrationDepositRequired -> "Stake address deposit required."

newtype StakeAddressDelegationError = VoteDelegationNotSupported AnyShelleyToBabbageEra deriving Show

instance Error StakeAddressDelegationError where
  displayError = \case
    VoteDelegationNotSupported (AnyShelleyToBabbageEra stbe) -> "Vote delegation not supported in " <> eraTxt stbe <> " era."
      where
        eraTxt :: forall era. ShelleyToBabbageEra era -> String
        eraTxt stbe' = shelleyToBabbageEraConstraints stbe' $
          Text.unpack . renderEra $ AnyCardanoEra (cardanoEra @era)

