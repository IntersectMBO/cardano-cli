{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Cip.Cip129.Conversion
  ( encodeCip129DrepVerficationKeyText 
  )
where

import Cardano.CLI.Read.DRep
import Data.Text
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger (StandardCrypto)
import Cardano.Api.Shelley


encodeCip129DrepVerficationKeyText :: AnyDrepVerificationKey -> Text
encodeCip129DrepVerficationKeyText = serialiseToBech32CIP129 . anyDrepVerificationKeyToCredential 


anyDrepVerificationKeyToCredential :: AnyDrepVerificationKey -> L.Credential L.DRepRole StandardCrypto
anyDrepVerificationKeyToCredential drepKey = 
    case drepKey of 
        AnyDrepVerificationKey vk -> 
            let DRepKeyHash hash = verificationKeyHash vk 
            in L.KeyHashObj hash
        AnyDrepExtendedVerificationKey vk ->
            let DRepExtendedKeyHash hash = verificationKeyHash vk 
            in L.KeyHashObj hash