
module Cardano.CLI.Types.Errors.EraBasedVoteError
  ( EraBasedVoteError(..)
  ) where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)

data EraBasedVoteError
  = EraBasedVoteReadError !(FileError InputDecodeError)
  | EraBasedVotingCredentialDecodeError !DecoderError
  | EraBasedVoteWriteError !(FileError ())
