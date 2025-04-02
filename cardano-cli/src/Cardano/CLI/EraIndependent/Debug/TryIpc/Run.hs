{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.TryIpc.Run
  ( runTryIpcCmd
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Api.Network qualified as Network
import Cardano.Api.Shelley qualified as Shelley

import Prelude

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set qualified as Set

runTryIpcCmd :: IO ()
runTryIpcCmd = do
  -- We set the connection parameters
  let connectionInfo =
        Api.LocalNodeConnectInfo
          { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 86_400) -- from shelley-genesis.json "epochLength" (for mainnet 432_000)
          , Api.localNodeNetworkId = Api.Testnet (Api.NetworkMagic 2) -- from shelley-genesis.json "netorkMagic"
          , Api.localNodeSocketPath = Api.File "/Users/palas/preview/state-node-preview/node.socket" -- socket file provided by the node
          }
  -- We make a query to obtain the current era
  eEra <- runExceptT $ Api.queryNodeLocalState connectionInfo Network.VolatileTip Api.QueryCurrentEra

  -- We unwrap the error and make sure it is not Byron
  Api.AnyShelleyBasedEra sbe :: Api.AnyShelleyBasedEra <- case eEra of
    Right (Api.AnyCardanoEra era) ->
      Api.caseByronOrShelleyBasedEra
        (error "We are in Byron era")
        (return . Api.AnyShelleyBasedEra)
        era
    Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"

  -- -- We make a query to obtain the number of UTxOs at the moment
  -- eUtxo <-
  --   runExceptT $
  --     Api.queryNodeLocalState
  --       connectionInfo
  --       Network.VolatileTip
  --       (Api.QueryInEra (Api.QueryInShelleyBasedEra sbe (Api.QueryUTxO Api.QueryUTxOWhole)))

  -- -- We unwrap the error and print the number of UTxOs
  -- case eUtxo of
  --   Right (Right (UTxO utxo)) -> do
  --     putStrLn $ "Number of UTxOs: " ++ show (length utxo)
  --   Right (Left (EraMismatch{ledgerEraName, otherEraName})) ->
  --     error ("Error we assumed era was " ++ show otherEraName ++ " but it was " ++ show ledgerEraName)
  --   Left AFPointTooOld -> error "Error, point queried in the chain is too old!"
  --   Left AFPointNotOnChain -> error "Error, point queried is not on chain!"

  let address = case Api.deserialiseFromBech32
        (Api.AsAddress Api.AsShelleyAddr)
        "addr_test1qqc536zmzggkvkes4lf2jpjxw5g0488f656yqlwpuw0uu3fk9tjxma0kpssjkg2e9ltgrl0tvfqay0lr7k4qzdyqya3s6udgu7" of
        Right addr -> addr
        Left err -> error $ "Error deserialising address: " ++ show err

  let destAddress = case Api.deserialiseFromBech32
        (Api.AsAddress Api.AsShelleyAddr)
        "addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99" of
        Right addr -> addr
        Left err -> error $ "Error deserialising address: " ++ show err

  -- We make a query to obtain the UTxOs for the given address
  eUtxo <-
    runExceptT $
      Api.queryNodeLocalState
        connectionInfo
        Network.VolatileTip
        ( Api.QueryInEra
            ( Api.QueryInShelleyBasedEra
                sbe
                (Api.QueryUTxO (Api.QueryUTxOByAddress $ Set.singleton $ Api.AddressShelley address))
            )
        )

  -- We unwrap the error and print the number of UTxOs
  utxo <- case eUtxo of
    Right (Right (Api.UTxO utxo)) -> do
      return utxo
    Right (Left (Consensus.EraMismatch{Consensus.ledgerEraName, Consensus.otherEraName})) ->
      error
        ( "Error, we assumed era was "
            ++ show otherEraName
            ++ " but it was "
            ++ show ledgerEraName
        )
    Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"

  when (Map.null utxo) $
    error "Error, no UTxOs found for the given address!"

  let txIns =
        [ ( txIn
          , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
          )
        | (txIn, _) <- Map.toList utxo
        ]

  let feeAmount = 200_000

  let totalValue =
        sum
          [ Api.txOutValueToLovelace txOutValue
          | (_, Api.TxOut _ txOutValue _ _) <- Map.toList utxo
          ]
          - feeAmount

  let txOut =
        Api.TxOut
          ( Api.AddressInEra
              (Api.ShelleyAddressInEra sbe)
              destAddress
          )
          (Api.lovelaceToTxOutValue sbe totalValue)
          Api.TxOutDatumNone
          Shelley.ReferenceScriptNone

  let txFee = Api.TxFeeExplicit sbe feeAmount

  let txBodyContent =
        Api.defaultTxBodyContent sbe
          & Api.setTxIns txIns
          & Api.setTxOuts [txOut]
          & Api.setTxFee txFee
  eSigningKey <-
    Api.readFileTextEnvelope
      (Api.AsSigningKey Api.AsPaymentKey)
      "/Users/palas/work/cardano-cli/test/address1/key.skey"

  let signingKey = case eSigningKey of
        Right sk -> sk
        Left err -> error $ "Error deserialising signing key: " ++ show err

  let witness = Api.WitnessPaymentKey signingKey

  let eTxBody = Api.createTransactionBody sbe txBodyContent

  let txBody = case eTxBody of
        Right txBody' -> txBody'
        Left err -> error $ "Error creating transaction body: " ++ show err

  let signedTx = Api.signShelleyTransaction sbe txBody [witness]

  result <- Api.submitTxToNodeLocal connectionInfo (Api.TxInMode sbe signedTx)

  case result of
    Api.SubmitSuccess -> putStrLn "Transaction submitted successfully!"
    Api.SubmitFail reason -> error $ "Error submitting transaction: " ++ show reason

  return ()
