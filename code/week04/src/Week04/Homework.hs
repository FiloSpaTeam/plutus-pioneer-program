{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx

handlePayContract :: Contract () PaySchema Text ()
handlePayContract = do
    Contract.handleError
        (\err -> Contract.logError $ "cannot execute contract! " ++ unpack err)
        payContract
    handlePayContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.EmulatorTrace
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace a b = do
    h <- activateContractWallet (Wallet 1) handlePayContract

    let recipient = (pubKeyHash $ walletPubKey $ Wallet 2)
    let ppa = PayParams {
                ppRecipient = recipient,
                ppLovelace = a
            }
    callEndpoint @"pay" h ppa
    void $ Emulator.waitNSlots 1

    let ppb = PayParams {
                ppRecipient = recipient,
                ppLovelace = b
            }

    callEndpoint @"pay" h ppb
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000

payTest3 :: IO ()
payTest3 = runEmulatorTraceIO $ payTrace 1000000000 1000000000

payTest4 :: IO ()
payTest4 = runEmulatorTraceIO $ payTrace 2000000 1000000000
