{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

#if !defined(__GLASGOW_HASKELL_LLVM__)
module Data.Primitive.Multi {-# WARNING "Only useful with the LLVM back end" #-} where
#else /* defined(__GLASGOW_HASKELL_LLVM__) */
module Data.Primitive.Multi (
    MultiType(..),
    Multi(..),
    MultiPrim(..),
    FloatX4(..),
    DoubleX2(..),
    Int32X4(..),
    Int64X2(..),

    indexByteArrayAsMulti,
    readByteArrayAsMulti,
    writeByteArrayAsMulti,

    indexOffAddrAsMulti,
    readOffAddrAsMulti,
    writeOffAddrAsMulti,

    peekElemOffAsMulti,
    pokeElemOffAsMulti,

    muncurry,

    prefetchPtrData,
    prefetchForeignPtrData,
    prefetchByteArrayData
 ) where

import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.Multi.Types
import Foreign.Storable
import GHC.Prim
import GHC.Base (Int(..))
import GHC.Float (Float(..), Double(..))
import GHC.Int (Int32(..), Int64(..))
import GHC.ForeignPtr
import GHC.Ptr

class MultiType a where
    data Multi a

    -- | The number of elements of type @a@ in a @Multi a@.
    multiplicity :: Multi a -> Int

    -- | A @Multi a@ containing the values @0@, @1@, ..., @multiplicity - 1@.
    multienum :: Multi a

    -- | Replicate a scalar across a @Multi a@.
    multireplicate :: a -> Multi a

    -- | Map a function over the elements of a @Multi a@.
    multimap :: (a -> a) -> Multi a -> Multi a

    -- | Fold a function over the elements of a @Multi a@.
    multifold :: (b -> a -> b) -> b -> Multi a -> b

    -- | Zip two @Multi a@'s with a function.
    multizipWith :: (a -> a -> a) -> Multi a -> Multi a -> Multi a

instance MultiType a => MultiType (a, a) where
    data Multi (a, a) = M_2 !(Multi a) !(Multi a)

    multiplicity _ = multiplicity (undefined :: Multi a)

    multienum = M_2 multienum multienum

    multireplicate (x, y) = M_2 (multireplicate x) (multireplicate y)

instance MultiType a => MultiType (a, a, a) where
    data Multi (a, a, a) = M_3 !(Multi a) !(Multi a) !(Multi a)

    multiplicity _ = multiplicity (undefined :: Multi a)

    multienum = M_3 multienum multienum multienum

    multireplicate (x, y, z) = M_3 (multireplicate x) (multireplicate y) (multireplicate z)

muncurry :: (Multi a -> Multi a -> b)
         -> (Multi (a, a) -> b)
muncurry f (M_2 x y) = f x y

class (Prim a, MultiType a, Prim (Multi a)) => MultiPrim a where
    -- | Read a multi-value from the array. The offset is in elements of type
    -- @a@ rather than in elements of type @Multi a@.
    indexByteArrayAsMulti# :: ByteArray# -> Int# -> Multi a

    -- | Read a multi-value from the mutable array. The offset is in elements of
    -- type @a@ rather than in elements of type @Multi a@.
    readByteArrayAsMulti# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Multi a #)

    -- | Write a multi-value to the mutable array. The offset is in elements of
    -- type @a@ rather than in elements of type @Multi a@.
    writeByteArrayAsMulti# :: MutableByteArray# s -> Int# -> Multi a -> State# s -> State# s

    -- | Read a multi-value from a memory position given by an address and an
    -- offset.  The memory block the address refers to must be immutable. The
    -- offset is in elements of type @a@ rather than in elements of type @Multi
    -- a@.
    indexOffAddrAsMulti# :: Addr# -> Int# -> Multi a

    -- | Read a multi-value from a memory position given by an address and an
    -- offset.  The offset is in elements of type @a@ rather than in elements of
    -- type @Multi a@.
    readOffAddrAsMulti# :: Addr# -> Int# -> State# s -> (# State# s, Multi a #)

    -- | Write a multi-value to a memory position given by an address and an
    -- offset.  The offset is in elements of type @a@ rather than in elements of
    -- type @Multi a@.
    writeOffAddrAsMulti# :: Addr# -> Int# -> Multi a -> State# s -> State# s

#define deriveMultiPrim(ty, mctr, mty, ctr, idx_arr, rd_arr, wr_arr, idx_addr, rd_addr, wr_addr) \
instance MultiPrim ty where {                                                 \
  indexByteArrayAsMulti# arr# i# = mctr (ctr (idx_arr arr# i#))               \
; readByteArrayAsMulti#  arr# i# s# = case rd_arr arr# i# s# of               \
                        { (# s1#, x# #) -> (# s1#, mctr (ctr x#) #) }         \
; writeByteArrayAsMulti# arr# i# (mctr (ctr x#)) s# = wr_arr arr# i# x# s#    \
                                                                              \
; indexOffAddrAsMulti# addr# i# = mctr (ctr (idx_addr addr# i#))              \
; readOffAddrAsMulti#  addr# i# s# = case rd_addr addr# i# s# of              \
                        { (# s1#, x# #) -> (# s1#, mctr (ctr x#) #) }         \
; writeOffAddrAsMulti# addr# i# (mctr (ctr x#)) s# = wr_addr addr# i# x# s#   \
};                                                                            \
instance MultiType ty where {                                                 \
  newtype Multi ty = mctr mty deriving (Prim, Num, Show, Storable)            \
; multiplicity _ = I# (sizeOf# (undefined :: Multi ty)) `quot`                \
                   I# (sizeOf# (undefined :: ty))

deriveMultiPrim(Float, MultiFloat, FloatX4, FX4#,
                indexFloatArrayAsFloatX4#,
                readFloatArrayAsFloatX4#,
                writeFloatArrayAsFloatX4#,
                indexFloatOffAddrAsFloatX4#,
                readFloatOffAddrAsFloatX4#,
                writeFloatOffAddrAsFloatX4#)
; multienum =
    MultiFloat (FX4# (packFloatX4# (# 0.0#, 1.0#, 2.0#, 3.0# #)))
; multireplicate (F# x#) =
    MultiFloat (FX4# (packFloatX4# (# x#, x#, x#, x# #)))
; multimap f (MultiFloat (FX4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackFloatX4# v#
        !(F# w'#)             = f (F# w#)
        !(F# x'#)             = f (F# x#)
        !(F# y'#)             = f (F# y#)
        !(F# z'#)             = f (F# z#)
        !v'#                  = packFloatX4# (# w'#, x'#, y'#, z'# #)
    in
      MultiFloat (FX4# v'#)
; multifold f z (MultiFloat (FX4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackFloatX4# v#
    in
      (((z `f` (F# w#)) `f` (F# x#)) `f` (F# y#)) `f` (F# z#)
; multizipWith f (MultiFloat (FX4# v1#)) (MultiFloat (FX4# v2#)) =
    let !(# w1#, x1#, y1#, z1# #) = unpackFloatX4# v1#
        !(# w2#, x2#, y2#, z2# #) = unpackFloatX4# v2#
        !(F# w'#)                 = f (F# w1#) (F# w2#)
        !(F# x'#)                 = f (F# x1#) (F# x2#)
        !(F# y'#)                 = f (F# y1#) (F# y2#)
        !(F# z'#)                 = f (F# z1#) (F# z2#)
        !v'#                      = packFloatX4# (# w'#, x'#, y'#, z'# #)
    in
      MultiFloat (FX4# v'#)
}

deriveMultiPrim(Double, MultiDouble, DoubleX2, DX2#,
                indexDoubleArrayAsDoubleX2#,
                readDoubleArrayAsDoubleX2#,
                writeDoubleArrayAsDoubleX2#,
                indexDoubleOffAddrAsDoubleX2#,
                readDoubleOffAddrAsDoubleX2#,
                writeDoubleOffAddrAsDoubleX2#)
; multienum =
    MultiDouble (DX2# (packDoubleX2# (# 0.0##, 1.0## #)))
; multireplicate (D# x#) =
    MultiDouble (DX2# (packDoubleX2# (# x#, x# #)))
; multimap f (MultiDouble (DX2# v#)) =
    let !(# x#, y# #) = unpackDoubleX2# v#
        !(D# x'#)             = f (D# x#)
        !(D# y'#)             = f (D# y#)
        !v'#                  = packDoubleX2# (# x'#, y'# #)
    in
      MultiDouble (DX2# v'#)
; multifold f z (MultiDouble (DX2# v#)) =
    let !(# x#, y# #) = unpackDoubleX2# v#
    in
      (z `f` (D# x#)) `f` (D# y#)
; multizipWith f (MultiDouble (DX2# v1#)) (MultiDouble (DX2# v2#)) =
    let !(# w1#, x1# #) = unpackDoubleX2# v1#
        !(# w2#, x2# #) = unpackDoubleX2# v2#
        !(D# w'#)       = f (D# w1#) (D# w2#)
        !(D# x'#)       = f (D# x1#) (D# x2#)
        !v'#            = packDoubleX2# (# w'#, x'# #)
    in
      MultiDouble (DX2# v'#)
}

deriveMultiPrim(Int32, MultiInt32, Int32X4, I32X4#,
                indexInt32ArrayAsInt32X4#,
                readInt32ArrayAsInt32X4#,
                writeInt32ArrayAsInt32X4#,
                indexInt32OffAddrAsInt32X4#,
                readInt32OffAddrAsInt32X4#,
                writeInt32OffAddrAsInt32X4#)
; multienum =
    MultiInt32 (I32X4# (packInt32X4# (# 0#, 1#, 2#, 3# #)))
; multireplicate (I32# x#) =
    MultiInt32 (I32X4# (packInt32X4# (# x#, x#, x#, x# #)))
; multimap f (MultiInt32 (I32X4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackInt32X4# v#
        !(I32# w'#)           = f (I32# w#)
        !(I32# x'#)           = f (I32# x#)
        !(I32# y'#)           = f (I32# y#)
        !(I32# z'#)           = f (I32# z#)
        !v'#                  = packInt32X4# (# w'#, x'#, y'#, z'# #)
    in
      MultiInt32 (I32X4# v'#)
; multifold f z (MultiInt32 (I32X4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackInt32X4# v#
    in
      (((z `f` (I32# w#)) `f` (I32# x#)) `f` (I32# y#)) `f` (I32# z#)
; multizipWith f (MultiInt32 (I32X4# v1#)) (MultiInt32 (I32X4# v2#)) =
    let !(# w1#, x1#, y1#, z1# #) = unpackInt32X4# v1#
        !(# w2#, x2#, y2#, z2# #) = unpackInt32X4# v2#
        !(I32# w'#)               = f (I32# w1#) (I32# w2#)
        !(I32# x'#)               = f (I32# x1#) (I32# x2#)
        !(I32# y'#)               = f (I32# y1#) (I32# y2#)
        !(I32# z'#)               = f (I32# z1#) (I32# z2#)
        !v'#                      = packInt32X4# (# w'#, x'#, y'#, z'# #)
    in
      MultiInt32 (I32X4# v'#)
}

deriveMultiPrim(Int64, MultiInt64, Int64X2, I64X2#,
                indexInt64ArrayAsInt64X2#,
                readInt64ArrayAsInt64X2#,
                writeInt64ArrayAsInt64X2#,
                indexInt64OffAddrAsInt64X2#,
                readInt64OffAddrAsInt64X2#,
                writeInt64OffAddrAsInt64X2#)
; multienum =
    MultiInt64 (I64X2# (packInt64X2# (# 0#, 1# #)))
; multireplicate (I64# x#) =
    MultiInt64 (I64X2# (packInt64X2# (# x#, x# #)))
; multimap f (MultiInt64 (I64X2# v#)) =
    let !(# x#, y# #) = unpackInt64X2# v#
        !(I64# x'#)   = f (I64# x#)
        !(I64# y'#)   = f (I64# y#)
        !v'#          = packInt64X2# (# x'#, y'# #)
    in
      MultiInt64 (I64X2# v'#)
; multifold f z (MultiInt64 (I64X2# v#)) =
    let !(# x#, y# #) = unpackInt64X2# v#
    in
      (z `f` (I64# x#)) `f` (I64# y#)
; multizipWith f (MultiInt64 (I64X2# v1#)) (MultiInt64 (I64X2# v2#)) =
    let !(# w1#, x1# #) = unpackInt64X2# v1#
        !(# w2#, x2# #) = unpackInt64X2# v2#
        !(I64# w'#)     = f (I64# w1#) (I64# w2#)
        !(I64# x'#)     = f (I64# x1#) (I64# x2#)
        !v'#            = packInt64X2# (# w'#, x'# #)
    in
      MultiInt64 (I64X2# v'#)
}

#if WORD_SIZE_IN_BITS == 32
deriveMultiPrim(Int, MultiInt, Int32X4, I32X4#,
                indexInt32ArrayAsInt32X4#,
                readInt32ArrayAsInt32X4#,
                writeInt32ArrayAsInt32X4#,
                indexInt32OffAddrAsInt32X4#,
                readInt32OffAddrAsInt32X4#,
                writeInt32OffAddrAsInt32X4#)
; multienum =
    MultiInt (I32X4# (packInt32X4# (# 0#, 1#, 2#, 3# #)))
; multireplicate (I# x#) =
    MultiInt (I32X4# (packInt32X4# (# x#, x#, x#, x# #)))
; multimap f (MultiInt (I32X4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackInt32X4# v#
        !(I# w'#)             = f (I# w#)
        !(I# x'#)             = f (I# x#)
        !(I# y'#)             = f (I# y#)
        !(I# z'#)             = f (I# z#)
        !v'#                  = packInt32X4# (# w'#, x'#, y'#, z'# #)
    in
      MultiInt (I32X4# v'#)
; multifold f z (MultiInt (I32X4# v#)) =
    let !(# w#, x#, y#, z# #) = unpackInt32X4# v#
    in
      (((z `f` (I# w#)) `f` (I# x#)) `f` (I# y#)) `f` (I# z#)
; multizipWith f (MultiInt (I32X4# v1#)) (MultiInt (I32X4# v2#)) =
    let !(# w1#, x1#, y1#, z1# #) = unpackInt32X4# v1#
        !(# w2#, x2#, y2#, z2# #) = unpackInt32X4# v2#
        !(I# w'#)                 = f (I# w1#) (I# w2#)
        !(I# x'#)                 = f (I# x1#) (I# x2#)
        !(I# y'#)                 = f (I# y1#) (I# y2#)
        !(I# z'#)                 = f (I# z1#) (I# z2#)
        !v'#                      = packInt32X4# (# w'#, x'#, y'#, z'# #)
    in
      MultiInt (I32X4# v'#)
}
#elif WORD_SIZE_IN_BITS == 64
deriveMultiPrim(Int, MultiInt, Int64X2, I64X2#,
                indexInt64ArrayAsInt64X2#,
                readInt64ArrayAsInt64X2#,
                writeInt64ArrayAsInt64X2#,
                indexInt64OffAddrAsInt64X2#,
                readInt64OffAddrAsInt64X2#,
                writeInt64OffAddrAsInt64X2#)
; multienum =
    MultiInt (I64X2# (packInt64X2# (# 0#, 1# #)))
; multireplicate (I# x#) =
    MultiInt (I64X2# (packInt64X2# (# x#, x# #)))
; multimap f (MultiInt (I64X2# v#)) =
    let !(# x#, y# #) = unpackInt64X2# v#
        !(I# x'#)     = f (I# x#)
        !(I# y'#)     = f (I# y#)
        !v'#          = packInt64X2# (# x'#, y'# #)
    in
      MultiInt (I64X2# v'#)
; multifold f z (MultiInt (I64X2# v#)) =
    let !(# x#, y# #) = unpackInt64X2# v#
    in
      (z `f` (I# x#)) `f` (I# y#)
; multizipWith f (MultiInt (I64X2# v1#)) (MultiInt (I64X2# v2#)) =
    let !(# w1#, x1# #) = unpackInt64X2# v1#
        !(# w2#, x2# #) = unpackInt64X2# v2#
        !(I# w'#)       = f (I# w1#) (I# w2#)
        !(I# x'#)       = f (I# x1#) (I# x2#)
        !v'#            = packInt64X2# (# w'#, x'# #)
    in
      MultiInt (I64X2# v'#)
}
#endif

-- | Read a primitive multi-value from the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
indexByteArrayAsMulti :: MultiPrim a => ByteArray -> Int -> Multi a
{-# INLINE indexByteArrayAsMulti #-}
indexByteArrayAsMulti (ByteArray arr#) (I# i#) =
    indexByteArrayAsMulti# arr# i#

-- | Read a primitive multi-value from the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
readByteArrayAsMulti :: (MultiPrim a, PrimMonad m)
                     => MutableByteArray (PrimState m) -> Int -> m (Multi a)
{-# INLINE readByteArrayAsMulti #-}
readByteArrayAsMulti (MutableByteArray arr#) (I# i#) =
    primitive (readByteArrayAsMulti# arr# i#)

-- | Write a primitive multi-value to the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
writeByteArrayAsMulti :: (MultiPrim a, PrimMonad m)
                      => MutableByteArray (PrimState m) -> Int -> Multi a -> m ()
{-# INLINE writeByteArrayAsMulti #-}
writeByteArrayAsMulti (MutableByteArray arr#) (I# i#) x =
    primitive_ (writeByteArrayAsMulti# arr# i# x)

-- | Read a multi-value from a memory position given by an address and an
-- offset.  The memory block the address refers to must be immutable. The offset
-- is in elements of type @a@ rather than in elements of type @Multi a@.
indexOffAddrAsMulti :: MultiPrim a => Addr -> Int -> Multi a
{-# INLINE indexOffAddrAsMulti #-}
indexOffAddrAsMulti (Addr addr#) (I# i#) =
    indexOffAddrAsMulti# addr# i#

-- | Read a multi-value from a memory position given by an address and an
-- offset.  The offset is in elements of type @a@ rather than in elements of
-- type @Multi a@.
readOffAddrAsMulti :: (MultiPrim a, PrimMonad m) => Addr -> Int -> m (Multi a)
{-# INLINE readOffAddrAsMulti #-}
readOffAddrAsMulti (Addr addr#) (I# i#) =
    primitive (readOffAddrAsMulti# addr# i#)

-- | Write a multi-value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than inelements of type @Multi
-- a@.
writeOffAddrAsMulti :: (MultiPrim a, PrimMonad m) => Addr -> Int -> Multi a -> m ()
{-# INLINE writeOffAddrAsMulti #-}
writeOffAddrAsMulti (Addr addr#) (I# i#) x =
    primitive_ (writeOffAddrAsMulti# addr# i# x)

peekElemOffAsMulti :: (MultiPrim a, PrimMonad m) => Ptr a -> Int -> m (Multi a)
{-# INLINE peekElemOffAsMulti #-}
peekElemOffAsMulti (Ptr a#) i = readOffAddrAsMulti (Addr a#) i

pokeElemOffAsMulti :: (MultiPrim a, PrimMonad m) => Ptr a -> Int -> Multi a -> m ()
{-# INLINE pokeElemOffAsMulti #-}
pokeElemOffAsMulti (Ptr a#) i x = writeOffAddrAsMulti (Addr a#) i x

prefetchPtrData :: Ptr a -> Int -> Ptr a
{-# INLINE prefetchPtrData #-}
prefetchPtrData (Ptr addr#) (I# i#) = Ptr (prefetchAddr# addr# i#)

prefetchForeignPtrData :: ForeignPtr a -> Int -> ForeignPtr a
{-# INLINE prefetchForeignPtrData #-}
prefetchForeignPtrData (ForeignPtr addr# fpc) (I# i#) = ForeignPtr (prefetchAddr# addr# i#) fpc

prefetchByteArrayData :: ByteArray -> Int -> ByteArray
{-# INLINE prefetchByteArrayData #-}
prefetchByteArrayData (ByteArray arr#) (I# i#) = ByteArray (prefetchByteArray# arr# i#)
#endif /* defined(__GLASGOW_HASKELL_LLVM__) */
