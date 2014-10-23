{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Grasp (
	Graspable(..), Grasp,
	empty, singleton, insert
) where

import qualified Data.Map  as M
import Data.Int  (Int8,  Int16,  Int32,  Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Maybe
import Data.Either
import Data.Binary
import Data.Typeable

data Graspable = GI8  Int8
               | GI16 Int16
               | GI32 Int32
               | GI64 Int64
               | GW8  Word8
               | GW16 Word16
               | GW32 Word32
               | GW64 Word64
               | GF32 Float
               | GF64 Double
               | GStr String
    deriving (Show, Ord, Eq)

instance Binary Graspable where
  put (GI8  a) = putWord8 0  >> put a
  put (GI16 a) = putWord8 1  >> put a
  put (GI32 a) = putWord8 2  >> put a
  put (GI64 a) = putWord8 3  >> put a
  put (GW8  a) = putWord8 4  >> put a
  put (GW16 a) = putWord8 5  >> put a
  put (GW32 a) = putWord8 6  >> put a
  put (GW64 a) = putWord8 7  >> put a
  put (GF32 a) = putWord8 8  >> put a
  put (GF64 a) = putWord8 9  >> put a
  put (GStr a) = putWord8 10 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0  -> get >>= \a -> return (GI8  a)
      1  -> get >>= \a -> return (GI16 a)
      2  -> get >>= \a -> return (GI32 a)
      3  -> get >>= \a -> return (GI64 a)
      4  -> get >>= \a -> return (GW8  a)
      5  -> get >>= \a -> return (GW16 a)
      6  -> get >>= \a -> return (GW32 a)
      7  -> get >>= \a -> return (GW64 a)
      8  -> get >>= \a -> return (GF32 a)
      9  -> get >>= \a -> return (GF64 a)
      10 -> get >>= \a -> return (GStr a)
      _ -> fail "no decoding"

newtype MapTree k v = MT (M.Map k (Maybe v, MapTree k v)) deriving (Show, Binary)
liftMT f (MT v) = MT $ f v

newtype Grasp = Grasp (MapTree Graspable [Graspable]) deriving (Show, Binary)
liftGrasp f (Grasp v) = Grasp $ f v

--instance Show Grasp where
--	show (Grasp m) show where 

empty :: Grasp
empty = Grasp $ MT $ M.empty

singleton :: [Graspable] -> [Graspable] -> Grasp
singleton ks vs = insert ks vs empty

insert :: [Graspable] -> [Graspable] -> Grasp -> Grasp
insert [] _  = error "grasp: cannot have a row with no keys"
insert ks vs = liftGrasp $ go ks vs
  where
    go :: [Graspable] -> [Graspable] -> (MapTree Graspable [Graspable]) -> (MapTree Graspable [Graspable])
    go [k]     vs = liftMT $ M.insertWith (_mod) k (Just vs, MT M.empty)
        where _mod (v, k) (v', k') = (v', k)
    go (k:ks)  vs = liftMT $ _mod (Just vs, MT M.empty) -- M.insertWith (fmap (go ks vs) . _mod) k (Nothing, MT M.empty)
        where _mod (v, k) (v', k') = (v', k)

testGrasp :: Grasp
testGrasp = singleton [GStr "Hello"] [GI32 58]

main = do print $ insert [GStr "World"] [] testGrasp