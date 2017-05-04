{-|
  This package provides block and block-related types.
-}
module Network.Haskoin.Block
(
  -- * Blocks
  Block(..)
, BlockLocator
, GetBlocks(..)
, BlockHeight
, Timestamp
, MinWork

  -- * Block Headers
, BlockHeader
, createBlockHeader
, blockVersion
, prevBlock
, merkleRoot
, blockTimestamp
, blockBits
, bhNonce
, headerHash
, GetHeaders(..)
, Headers(..)
, BlockHeaderCount
, BlockHash(..)
, blockHashToHex
, hexToBlockHash

  -- * Merkle Blocks
, MerkleBlock(..)
, MerkleRoot
, FlagBits
, PartialMerkleTree
, calcTreeHeight
, calcTreeWidth
, buildMerkleRoot
, calcHash
, buildPartialMerkle
, extractMatches

  -- * Difficulty Target
, decodeCompact
, encodeCompact

  -- * Block Validation
, validateBlock
, diffInterval
, headerWork
, isMinWork

  -- * Genesis
, genesisBlock
) where

import           Network.Haskoin.Block.Genesis
import           Network.Haskoin.Block.Merkle
import           Network.Haskoin.Block.Types
import           Network.Haskoin.Block.Validation
