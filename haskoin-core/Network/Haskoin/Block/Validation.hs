module Network.Haskoin.Block.Validation where

import           Control.Monad               (unless, when)
import           Data.Bits                   (shiftL)
import qualified Data.ByteString             as BS
import qualified Data.IntMap.Strict          as M (IntMap, fromList, lookup)
import           Data.List                   (sort)
import           Data.Serialize              (encode)
import           Data.Word                   (Word32)
import           Network.Haskoin.Block.Types
import           Network.Haskoin.Constants
import           Network.Haskoin.Util

-- | Checkpoints from bitcoind reference implementation /src/checkpoints.cpp
-- presented as an IntMap.
checkpointMap :: M.IntMap BlockHash
checkpointMap = M.fromList checkpoints

-- | Verify that a block hash at a given height either matches an existing
-- checkpoint or is not a checkpoint.
verifyCheckpoint :: BlockHeight -> BlockHash -> Bool
verifyCheckpoint height hash =
    case M.lookup (fromEnum height) checkpointMap of
        Just value -> hash == value
        Nothing    -> True

-- | Verify that block header conforms to protocol.
validateBlock :: BlockHeight
              -- ^ Parent block height
              -> BlockHeader
              -- ^ Parent block header
              -> [Timestamp]
              -- ^ Timestamps of previous 11 blocks
              -> Timestamp
              -- ^ Previous difficulty change
              -> Maybe Word32
              -- ^ Height of most recent checkpoint
              -> MinWork
              -- ^ Last MinWork (e.g. Testnet3)
              -> Timestamp
              -- ^ Current time
              -> BlockHeader
              -- ^ Block header to validate
              -> Either String ()
validateBlock parentHeight
              parentHeader
              timestamps
              prevDiffChange
              lastCheckPoint
              lastMinWork
              timestamp
              header = do
    unless (isValidPOW header) $
        Left "Invalid proof of work"

    unless (blockTimestamp header <= timestamp + 2 * 60 * 60) $
        Left "Invalid header timestamp"

    unless (blockBits header == nextWork) $
        Left "Incorrect work transition (bits)"

    when (blockTimestamp header <= medianTime) $
        Left "Block timestamp is too early"

    unless (maybe True (parentHeight + 1 >) lastCheckPoint) $
        Left "Rewriting pre-checkpoint chain"

    unless (verifyCheckpoint
            (fromIntegral $ parentHeight + 1)
            (headerHash header)) $
        Left "Rejected by checkpoint lock-in"

    when (fst bip34Block >= 0 &&
          fst bip34Block == fromEnum (parentHeight + 1) &&
          snd bip34Block /= headerHash header) $
        Left "Rejected because first BIP34 block has wrong hash"

    when (fst bip34Block >= 0 &&
          fromEnum (parentHeight + 1) > fst bip34Block
          && blockVersion header == 1) $
        Left "Rejected version 1 block"
  where
    nextWork = nextWorkRequired
               parentHeight parentHeader prevDiffChange lastMinWork header
    medianTime = sort timestamps !! (length timestamps `div` 2)

-- | Returns the work required for a block header given the previous block. This
-- coresponds to bitcoind function GetNextWorkRequired in main.cpp.
nextWorkRequired :: BlockHeight
                 -> BlockHeader
                 -> Timestamp
                 -> MinWork
                 -> BlockHeader
                 -> Word32
nextWorkRequired parentHeight parentHeader timestamp minWork header
    -- Genesis block
    | parentHeight == 0 = encodeCompact powLimit
    -- No difficulty adjustement
    | powNoRetargetting = blockBits parentHeader
    -- Only change the difficulty once per interval
    | (parentHeight + 1) `mod` diffInterval /= 0 =
          if allowMinDifficultyBlocks
          then minPOW
          else blockBits parentHeader
    | otherwise = workFromInterval timestamp parentHeader
  where
    delta = targetSpacing * 2
    minPOW
        | blockTimestamp header > blockTimestamp parentHeader + delta =
              encodeCompact powLimit
        | otherwise = minWork

-- | Computes the work required for the next block given a timestamp and the
-- current block. The timestamp should come from the block that matched the
-- last jump in difficulty (spaced out by 2016 blocks in prodnet).
workFromInterval :: Timestamp -> BlockHeader -> Word32
workFromInterval ts lastB
    | newDiff > powLimit = encodeCompact powLimit
    | otherwise          = encodeCompact newDiff
  where
    t = fromIntegral $ blockTimestamp lastB - ts
    actualTime
        | t < targetTimespan `div` 4 = targetTimespan `div` 4
        | t > targetTimespan * 4     = targetTimespan * 4
        | otherwise                  = t
    lastDiff = decodeCompact $ blockBits lastB
    newDiff = lastDiff * toInteger actualTime `div` toInteger targetTimespan

-- | Returns True if the difficulty target (bits) of the header is valid and the
-- proof of work of the header matches the advertised difficulty target. This
-- function corresponds to the function CheckProofOfWork from bitcoind in
-- main.cpp.
isValidPOW :: BlockHeader -> Bool
isValidPOW bh
    | target <= 0 || target > powLimit = False
    | otherwise = headerPOW bh <= fromIntegral target
  where
    target = decodeCompact $ blockBits bh

-- | Returns the proof of work of a block header as an Integer number.
headerPOW :: BlockHeader -> Integer
headerPOW =  bsToInteger . BS.reverse . encode . headerHash

-- | Returns the work represented by this block. Work is defined as the number
-- of tries needed to solve a block in the average case with respect to the
-- target.
headerWork :: BlockHeader -> Integer
headerWork bh =
    fromIntegral $ largestHash `div` (target + 1)
  where
    target      = decodeCompact (blockBits bh)
    largestHash = 1 `shiftL` 256

-- | Number of blocks on average between difficulty cycles (2016 blocks).
diffInterval :: Word32
diffInterval = targetTimespan `div` targetSpacing

isMinWork :: BlockHeight -> BlockHeader -> Bool
isMinWork height header
    | not allowMinDifficultyBlocks = True
    | height `mod` diffInterval == 0 = True
    | blockBits header /= encodeCompact powLimit = True
    | otherwise = False
