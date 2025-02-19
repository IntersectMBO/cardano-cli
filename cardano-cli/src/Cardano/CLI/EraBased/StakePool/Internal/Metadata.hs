module Cardano.CLI.EraBased.StakePool.Internal.Metadata
  ( carryHashChecks
  )
where

import Cardano.Api.Shelley

import Cardano.CLI.EraIndependent.Hash.Internal.Common hiding (carryHashChecks)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.StakePoolCmdError

import Control.Monad

-- | Check the hash of the anchor data against the hash in the anchor if
-- checkHash is set to CheckHash.
carryHashChecks
  :: PotentiallyCheckedAnchor StakePoolMetadataReference StakePoolMetadataReference
  -- ^ The information about anchor data and whether to check the hash (see 'PotentiallyCheckedAnchor')
  -> ExceptT StakePoolCmdError IO ()
carryHashChecks potentiallyCheckedAnchor =
  case pcaMustCheck potentiallyCheckedAnchor of
    CheckHash -> do
      let urlText = stakePoolMetadataURL anchor
      metadataBytes <-
        withExceptT
          StakePoolCmdFetchURLError
          ( getByteStringFromURL
              httpsAndIpfsSchemes
              urlText
          )

      let expectedHash = stakePoolMetadataHash anchor

      (_metadata, metadataHash) <-
        firstExceptT StakePoolCmdMetadataValidationError
          . hoistEither
          $ validateAndHashStakePoolMetadata metadataBytes

      when (metadataHash /= expectedHash) $
        left $
          StakePoolCmdHashMismatchError expectedHash metadataHash
    TrustHash -> pure ()
 where
  anchor = pcaAnchor potentiallyCheckedAnchor
