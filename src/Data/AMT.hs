{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeFamilies          #-}
#endif
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields -ddump-simpl -dsuppress-all #-}

module Data.AMT where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative        (Applicative (pure), (<$>))
import           Data.Monoid                (Monoid (mappend, mempty))
import           Data.Traversable           (Traversable (..))
import           Data.Word                  (Word)
#endif
#if __GLASGOW_HASKELL__ >= 711
import           Data.Semigroup             (Semigroup ((<>)))
#endif
import           Control.DeepSeq            (NFData (rnf))
import           Control.Monad.ST           (ST)
import           Control.Monad.Trans.Except (Except, runExcept, throwE)
import           Data.Bits
import           Data.Data                  hiding (Typeable)
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as L
import           Data.Word                  (Word64)
import           GHC.Exts                   (build, reallyUnsafePtrEquality#,
                                             (==#))
import           Prelude                    hiding (filter, foldr, lookup, map,
                                             null, pred)
import           Text.Read                  hiding (step)

import qualified Data.HashMap.Array         as A
import           Data.Typeable              (Typeable)

#if __GLASGOW_HASKELL__ >= 707
import           GHC.Exts                   (isTrue#)
#endif
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts                   as Exts
#endif

import qualified Numeric

#if MIN_VERSION_base(4,9,0)
import           Data.Functor.Classes
#endif

-- $setup
-- >>> :set -XBinaryLiterals
-- >>> import qualified Data.List as L

type Key = Word64
type Prefix = Key
type Mask = Key
type Bitmap = Word64

data AMT a
  = Empty
  | Leaf !Key !a
  | Inner !Prefix !Mask !Bitmap !(A.Array (AMT a))
  | Full !Prefix !Mask !(A.Array (AMT a))
  deriving Eq


instance Show a => Show (AMT a) where
  show amt = "toList " ++ show (toList amt)


instance NFData a => NFData (AMT a) where
  rnf = \case
    Empty -> ()
    Leaf k v -> rnf v
    Inner p m bm cs -> rnf p `seq` rnf m `seq` rnf bm `seq` rnf cs
    Full p m cs -> rnf p `seq` rnf m `seq` rnf cs


-- * Bit twiddling

bitsPerSubKey :: Int
bitsPerSubKey = 6

-- | Does the key @i@ match the prefix @p@ (up to but not including
-- Mask @m@)?
match :: Key -> Prefix -> Mask -> Bool
match i p m = clz == 0 || shift i == shift p
  where
    clz = countLeadingZeros m
    -- shifting right by >= `finiteBitSize m` is undefined
    shift w = unsafeShiftR w (finiteBitSize m - clz)
{-# INLINE match #-}

-- |
-- >>> longestCommonPrefix 0 1
-- (0,63)
-- >>> longestCommonPrefix 128 1
-- (0,4032)
-- >>> longestCommonPrefix 128 129
-- (128,63)
-- >>> longestCommonPrefix 4096 4224
-- (4096,4032)
longestCommonPrefix :: Prefix -> Prefix -> (Prefix, Mask)
longestCommonPrefix p1 p2 = (lcp, mask)
  where
    diff = p1 `xor` p2
    firstDivergence = log2 diff
    -- `firstDivergence` is the index of the most significant
    -- diverging bit counting from the right (e.g. the least
    -- significant side).
    -- In order not to run into complicated cases requiring
    -- inefficient merging of inner nodes, we floor the
    -- `firstDivergence` to the next multiple of `bitsPerSubKey`,
    -- which then is the number of bits to shift `mask0` by.
    floorToNextMultipleOf n i = (i `div` n) * n
    shift = floorToNextMultipleOf bitsPerSubKey firstDivergence
    mask0 = bit bitsPerSubKey - 1 -- 6 bits => 63 = 0b0111111
    mask = unsafeShiftL mask0 shift
    -- We can get the lcp by masking with all bits above the mask
    lcp = p1 .&. complement (bit (shift + bitsPerSubKey) - 1)
{-# INLINE longestCommonPrefix #-}

-- | Given a 'Word64' \(w\) this function computes an 'Int' \(b\) such that
-- \(2^b <= w < 2^(b+1)\). Note the exceptional case for @log2 0@.
--
-- >>> log2 8
-- 3
-- >>> log2 42
-- 5
-- >>> log2 0
-- -1
log2 :: Word64 -> Int
log2 w = finiteBitSize w - 1 - countLeadingZeros w
{-# INLINE log2 #-}

-- | Masks the given 'Key' with the 'Mask' and shifts the result to the right
-- according to the number of trailing zeros in the mask.
-- Effictively, this reads out the masked bits and \'converts\' them into a
-- number.
--
-- >>> maskedBitsUnshifted 0b10101010 0b00011100 == 0b00000010
-- True
maskedBitsUnshifted :: Key -> Mask -> Int
maskedBitsUnshifted k m =
  fromIntegral (unsafeShiftR (k .&. m) (countTrailingZeros m))
{-# INLINE maskedBitsUnshifted #-}

-- | @sparseIndex bm i@ counts the number of ones in @bm@ up to, and
-- including, the @i@th bit (counting 1-based).
--
-- >>> sparseIndex 0b10110110 0
-- 0
-- >>> sparseIndex 0b10110110 1
-- 0
-- >>> sparseIndex 0b10110110 2
-- 1
-- >>> sparseIndex 0b10110110 3
-- 2
-- >>> sparseIndex 0b10110110 4
-- 2
-- >>> sparseIndex 0b10110110 5
-- 3
sparseIndex :: Bitmap -> Int -> Int
sparseIndex bm i = popCount (bm .&. (bit i - 1))
{-# INLINE sparseIndex #-}

-- * Operations

empty :: AMT a
empty = Empty

lookup :: Key -> AMT a -> Maybe a
lookup !k = go
  where
    go = \case
      Leaf k' v
        | k' == k
        -> Just v
      Inner p m bm cs
        | Right i <- computeChildIndex k p m bm
        -> go (A.index cs i)
      Full p m cs
        | let i = maskedBitsUnshifted p m
        -> go (A.index cs i)
      _ -> Nothing

insert :: Key -> a -> AMT a -> AMT a
insert !k v t = case t of
  Empty -> Leaf k v
  Leaf k' _
    | k' == k -> Leaf k v
    | otherwise -> link k (Leaf k v) k' t
  Inner p' m' bm' cs' ->
    case computeChildIndex k p' m' bm' of
      Right si -> Inner p' m' bm' (A.updateWith' cs' si (insert k v))
      Left PrefixDiverges -> link k (Leaf k v) p' t
      Left (NotAChild i)
        | bm' .|. bit i == complement 0
        , let si = maskedBitsUnshifted k m'
        -> Full p' m' (A.insert cs' si (Leaf k v))
        | let si = sparseIndex bm' i
        -> Inner p' m' (bm' .|. bit i) (A.insert cs' si (Leaf k v))
  Full p' m' cs' ->
    Full p' m' (A.updateWith' cs' (maskedBitsUnshifted k m') (insert k v))

link :: Prefix -> AMT a -> Prefix -> AMT a -> AMT a
link p1 t1 p2 t2 = Inner p m (bit i1 .|. bit i2) (A.fromList 2 children)
  where
    (p, m) = longestCommonPrefix p1 p2
    i1 = maskedBitsUnshifted p1 m
    i2 = maskedBitsUnshifted p2 m
    children = if i1 < i2 then [t1, t2] else [t2, t1]
{-# INLINE link #-}

data MismatchReason
  = PrefixDiverges
  | NotAChild Int
  deriving (Eq, Show)

throwUnlessM :: e -> Bool -> Except e ()
throwUnlessM ex False = throwE ex
throwUnlessM _ _      = pure ()

-- |
-- >>> computeChildIndex 0b100 0b000 0b011 0
-- Left PrefixDiverges
-- >>> computeChildIndex 0b000 0b100 0b011 0
-- Left PrefixDiverges
-- >>> computeChildIndex 0b000 0b000 0b011 0b0101
-- Right 0
-- >>> computeChildIndex 0b001 0b000 0b011 0b0101
-- Left (NotAChild 1)
-- >>> computeChildIndex 0b010 0b000 0b011 0b0101
-- Right 1
-- >>> computeChildIndex 0b011 0b000 0b011 0b0101
-- Left (NotAChild 3)
computeChildIndex :: Key -> Prefix -> Mask -> Bitmap -> Either MismatchReason Int
computeChildIndex k p m bm = runExcept $ do
  throwUnlessM PrefixDiverges (match k p m)
  let i = maskedBitsUnshifted k m
  throwUnlessM (NotAChild i) (testBit bm i)
  pure (sparseIndex bm i)
{-# INLINE computeChildIndex #-}

toList :: AMT a -> [(Key, a)]
toList = \case
  Empty -> []
  Leaf k v -> [(k, v)]
  Inner _ _ _ cs -> concatMap toList (A.toList cs)

fromList :: [(Key, a)] -> AMT a
fromList = Foldable.foldr (uncurry insert) empty

showHex :: Word64 -> String
showHex = flip Numeric.showHex ""

showRepr :: Show a => AMT a -> String
showRepr = \case
  Empty -> "Empty"
  Leaf k v -> "Leaf " ++ showHex k ++ " " ++ show v
  Inner p m bm cs -> concat
    [ "Inner "
    , showHex p
    , " "
    , showHex m
    , " "
    , showHex bm
    , " ["
    , L.intercalate ", " (fmap showRepr (A.toList cs))
    , "]"
    ]
