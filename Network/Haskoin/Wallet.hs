{-|
  This package provides a command line application called /hw/ (haskoin
  wallet). It is a lightweight bitcoin wallet featuring BIP32 key management,
  deterministic signatures (RFC-6979) and first order support for
  multisignature transactions. A library API for /hw/ is also exposed.
-}
module Network.Haskoin.Wallet
( 
-- *Client
  clientMain
, OutputFormat(..)
, Config(..)

-- *Server
, runSPVServer
, stopSPVServer
, SPVMode(..)

-- *API JSON Types
, JsonKeyRing(..)
, JsonAccount(..)
, JsonAddr(..)
, JsonCoin(..)
, JsonTx(..)

-- *API Request Types
, WalletRequest(..)
, PageRequest(..)
, validPageRequest
, BalanceType(..)
, NewKeyRing(..)
, NewAccount(..)
, SetAccountGap(..)
, OfflineTxData(..)
, CoinSignData(..)
, TxAction(..)
, AddressLabel(..)
, NodeAction(..)
, AccountType(..)
, AddressType(..)
, addrTypeIndex
, TxType(..)
, TxConfidence(..)
, CoinStatus(..)

-- *API Response Types
, WalletResponse(..)
, MnemonicRes(..)
, TxHashConfidenceRes(..)
, TxConfidenceRes(..)
, TxCompleteRes(..)
, PageRes(..)
, RescanRes(..)
, TxRes(..)
, BalanceRes(..)

) where

import Network.Haskoin.Wallet.Client
import Network.Haskoin.Wallet.Server
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Types

