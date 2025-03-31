{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.TryIpc.Run
  ( runTryIpcCmd
  )
where

import Cardano.Api
import Cardano.Api.Consensus (EraMismatch (..))
import Cardano.Api.Network (Target (VolatileTip))
import Cardano.Api.Shelley
  ( AcquiringFailure (AFPointNotOnChain, AFPointTooOld)
  , ReferenceScript (ReferenceScriptNone)
  )

import Cardano.Prelude ((&))

import Control.Monad (when)
import Data.Map qualified as Map
import Data.Set qualified as Set

runTryIpcCmd :: IO ()
runTryIpcCmd = do
  -- We set the connection parameters
  let connectionInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 86_400) -- from shelley-genesis.json "epochLength" (for mainnet 432_000)
          , localNodeNetworkId = Testnet (NetworkMagic 2) -- from shelley-genesis.json "netorkMagic"
          , localNodeSocketPath = File "/Users/palas/preview/state-node-preview/node.socket" -- socket file provided by the node
          }
  -- We make a query to obtain the current era
  eEra <- runExceptT $ queryNodeLocalState connectionInfo VolatileTip QueryCurrentEra

  -- We unwrap the error and make sure it is not Byron
  AnyShelleyBasedEra sbe :: AnyShelleyBasedEra <- case eEra of
    Right (AnyCardanoEra era) ->
      caseByronOrShelleyBasedEra
        (error "We are in Byron era")
        (return . AnyShelleyBasedEra)
        era
    Left AFPointTooOld -> error "Error, point queried in the chain is too old!"
    Left AFPointNotOnChain -> error "Error, point queried is not on chain!"

  -- We make a query to obtain the number of UTxOs at the moment
  -- eUtxo <-
  --   runExceptT $
  --     queryNodeLocalState
  --       connectionInfo
  --       VolatileTip
  --       (QueryInEra (QueryInShelleyBasedEra sbe (QueryUTxO QueryUTxOWhole)))

  -- -- We unwrap the error and print the number of UTxOs
  -- case eUtxo of
  --   Right (Right (UTxO utxo)) -> do
  --     putStrLn $ "Number of UTxOs: " ++ show (length utxo)
  --   Right (Left (EraMismatch{ledgerEraName, otherEraName})) ->
  --     error ("Error we assumed era was " ++ show otherEraName ++ " but it was " ++ show ledgerEraName)
  --   Left AFPointTooOld -> error "Error, point queried in the chain is too old!"
  --   Left AFPointNotOnChain -> error "Error, point queried is not on chain!"

  let address = case deserialiseFromBech32
        (AsAddress AsShelleyAddr)
        "addr_test1qqc536zmzggkvkes4lf2jpjxw5g0488f656yqlwpuw0uu3fk9tjxma0kpssjkg2e9ltgrl0tvfqay0lr7k4qzdyqya3s6udgu7" of
        Right addr -> addr
        Left err -> error $ "Error deserialising address: " ++ show err

  let destAddress = case deserialiseFromBech32
        (AsAddress AsShelleyAddr)
        "addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99" of
        Right addr -> addr
        Left err -> error $ "Error deserialising address: " ++ show err

  -- We make a query to obtain the UTxOs for the given address
  eUtxo <-
    runExceptT $
      queryNodeLocalState
        connectionInfo
        VolatileTip
        ( QueryInEra
            (QueryInShelleyBasedEra sbe (QueryUTxO (QueryUTxOByAddress $ Set.singleton $ AddressShelley address)))
        )

  -- We unwrap the error and print the number of UTxOs
  utxo <- case eUtxo of
    Right (Right (UTxO utxo)) -> do
      return utxo
    Right (Left (EraMismatch{ledgerEraName, otherEraName})) ->
      error ("Error we assumed era was " ++ show otherEraName ++ " but it was " ++ show ledgerEraName)
    Left AFPointTooOld -> error "Error, point queried in the chain is too old!"
    Left AFPointNotOnChain -> error "Error, point queried is not on chain!"

  when (Map.null utxo) $
    error "Error, no UTxOs found for the given address!"

  let txIns =
        [ ( txIn
          , BuildTxWith (KeyWitness KeyWitnessForSpending)
          )
        | (txIn, _) <- Map.toList utxo
        ]

  let feeAmount = 200_000

  let totalValue =
        sum
          [ txOutValueToLovelace txOutValue
          | (_, TxOut _ txOutValue _ _) <- Map.toList utxo
          ]
          - feeAmount

  let txOut =
        TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              destAddress
          )
          (lovelaceToTxOutValue sbe totalValue)
          TxOutDatumNone
          ReferenceScriptNone

  let txFee = TxFeeExplicit sbe feeAmount

  let txBodyContent =
        defaultTxBodyContent sbe
          & setTxIns txIns
          & setTxOuts [txOut]
          & setTxFee txFee
  eSigningKey <-
    readFileTextEnvelope
      (AsSigningKey AsPaymentKey)
      "/Users/palas/work/cardano-cli/test/address1/key.skey"

  let signingKey = case eSigningKey of
        Right sk -> sk
        Left err -> error $ "Error deserialising signing key: " ++ show err

  let witness = WitnessPaymentKey signingKey

  let eTxBody = createTransactionBody sbe txBodyContent

  let txBody = case eTxBody of
        Right txBody' -> txBody'
        Left err -> error $ "Error creating transaction body: " ++ show err

  let signedTx = signShelleyTransaction sbe txBody [witness]

  result <- submitTxToNodeLocal connectionInfo (TxInMode sbe signedTx)

  case result of
    SubmitSuccess -> putStrLn "Transaction submitted successfully!"
    SubmitFail reason -> error $ "Error submitting transaction: " ++ show reason

  return ()
