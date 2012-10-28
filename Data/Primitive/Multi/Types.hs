{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

#if !defined(__GLASGOW_HASKELL_LLVM__)
module Data.Primitive.Multi.Types {-# WARNING "Only useful with the LLVM back end" #-} where
#else /* defined(__GLASGOW_HASKELL_LLVM__) */
module Data.Primitive.Multi.Types (
    FloatX4(..),
    DoubleX2(..),
    Int32X4(..),
    Int64X2(..)
  ) where

import Data.Primitive.MachDeps
import Foreign.Storable
import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.Types

data FloatX4 = FX4# FloatX4#

mapFloatX4 :: (Float -> Float) -> FloatX4 -> FloatX4
mapFloatX4 f (FX4# x#) =
    let !(# a#, b#, c#, d# #) = unpackFloatX4# x#
        !(F# a'#)             = f (F# a#)
        !(F# b'#)             = f (F# b#)
        !(F# c'#)             = f (F# c#)
        !(F# d'#)             = f (F# d#)
        !fx#                  = packFloatX4# a'# b'# c'# d'#
    in
      FX4# fx#

instance Num FloatX4 where
    x + y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `plusFloatX4#` y#)

    x - y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `minusFloatX4#` y#)

    x * y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `timesFloatX4#` y#)

    abs = mapFloatX4 abs

    signum = mapFloatX4 signum

    fromInteger i =
        let !(F# f#) = fromInteger i
            v#       = packFloatX4# f# f# f# f#
        in
          FX4# v#

instance Show FloatX4 where
    showsPrec _ (FX4# v#) =
        let !(# a#, b#, c#, d# #) = unpackFloatX4# v#
        in
          showString "<" . showv [F# a#, F# b#, F# c#, F# d#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Storable FloatX4 where
    sizeOf _     = 4*sIZEOF_FLOAT
    alignment _  = 4*sIZEOF_FLOAT
    peekElemOff  = readFloatX4OffPtr
    pokeElemOff  = writeFloatX4OffPtr

readFloatX4OffPtr :: Ptr FloatX4 -> Int -> IO FloatX4
readFloatX4OffPtr (Ptr a) (I# i) = IO $ \s ->
    case readFloatX4OffAddr# a i s of
      (# s2, x #) -> (# s2, FX4# x #)

writeFloatX4OffPtr :: Ptr FloatX4 -> Int -> FloatX4 -> IO ()
writeFloatX4OffPtr (Ptr a) (I# i) (FX4# x) = IO $ \s ->
    case writeFloatX4OffAddr# a i x s of
      s2 -> (# s2, () #)

data DoubleX2 = DX2# DoubleX2#

mapDoubleX2 :: (Double -> Double) -> DoubleX2 -> DoubleX2
mapDoubleX2 f (DX2# x#) =
    let !(# a#, b# #) = unpackDoubleX2# x#
        !(D# a'#)     = f (D# a#)
        !(D# b'#)     = f (D# b#)
        !fx#          = packDoubleX2# a'# b'#
    in
      DX2# fx#

instance Num DoubleX2 where
    x + y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `plusDoubleX2#` y#)

    x - y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `minusDoubleX2#` y#)

    x * y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `timesDoubleX2#` y#)

    abs = mapDoubleX2 abs

    signum = mapDoubleX2 signum

    fromInteger i =
        let !(D# f#) = fromInteger i
            v#       = packDoubleX2# f# f#
        in
          DX2# v#

instance Show DoubleX2 where
    showsPrec _ (DX2# v#) =
        let !(# a#, b# #) = unpackDoubleX2# v#
        in
          showString "<" . showv [D# a#, D# b#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Storable DoubleX2 where
    sizeOf _     = 2*sIZEOF_DOUBLE
    alignment _  = 2*sIZEOF_DOUBLE
    peekElemOff  = readDoubleX2OffPtr
    pokeElemOff  = writeDoubleX2OffPtr

readDoubleX2OffPtr :: Ptr DoubleX2 -> Int -> IO DoubleX2
readDoubleX2OffPtr (Ptr a) (I# i) = IO $ \s ->
    case readDoubleX2OffAddr# a i s of
      (# s2, x #) -> (# s2, DX2# x #)

writeDoubleX2OffPtr :: Ptr DoubleX2 -> Int -> DoubleX2 -> IO ()
writeDoubleX2OffPtr (Ptr a) (I# i) (DX2# x) = IO $ \s ->
    case writeDoubleX2OffAddr# a i x s of
      s2 -> (# s2, () #)

data Int32X4 = I32X4# Int32X4#

mapInt32X4 :: (Int32 -> Int32) -> Int32X4 -> Int32X4
mapInt32X4 f (I32X4# x#) =
    let !(# a#, b#, c#, d# #) = unpackInt32X4# x#
        !(I32# a'#)           = f (I32# a#)
        !(I32# b'#)           = f (I32# b#)
        !(I32# c'#)           = f (I32# c#)
        !(I32# d'#)           = f (I32# d#)
        !fx#                  = packInt32X4# a'# b'# c'# d'#
    in
      I32X4# fx#

instance Num Int32X4 where
    x + y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `plusInt32X4#` y#)

    x - y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `minusInt32X4#` y#)

    x * y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `timesInt32X4#` y#)

    abs = mapInt32X4 abs

    signum = mapInt32X4 signum

    fromInteger i =
        let !(I32# n#) = fromInteger i
            v#         = packInt32X4# n# n# n# n#
        in
          I32X4# v#

instance Show Int32X4 where
    showsPrec _ (I32X4# v#) =
        let !(# a#, b#, c#, d# #) = unpackInt32X4# v#
        in
          showString "<" . showv [I32# a#, I32# b#, I32# c#, I32# d#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Storable Int32X4 where
    sizeOf _     = 4*sIZEOF_INT32
    alignment _  = 4*sIZEOF_INT32
    peekElemOff  = readInt32X4OffPtr
    pokeElemOff  = writeInt32X4OffPtr

readInt32X4OffPtr :: Ptr Int32X4 -> Int -> IO Int32X4
readInt32X4OffPtr (Ptr a) (I# i) = IO $ \s ->
    case readInt32X4OffAddr# a i s of
      (# s2, x #) -> (# s2, I32X4# x #)

writeInt32X4OffPtr :: Ptr Int32X4 -> Int -> Int32X4 -> IO ()
writeInt32X4OffPtr (Ptr a) (I# i) (I32X4# x) = IO $ \s ->
    case writeInt32X4OffAddr# a i x s of
      s2 -> (# s2, () #)

data Int64X2 = I64X2# Int64X2#

mapInt64X2 :: (Int64 -> Int64) -> Int64X2 -> Int64X2
mapInt64X2 f (I64X2# x#) =
    let !(# a#, b# #) = unpackInt64X2# x#
        !(I64# a'#)   = f (I64# a#)
        !(I64# b'#)   = f (I64# b#)
        !fx#          = packInt64X2# a'# b'#
    in
      I64X2# fx#

instance Num Int64X2 where
    x + y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `plusInt64X2#` y#)

    x - y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `minusInt64X2#` y#)

    x * y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `timesInt64X2#` y#)

    abs = mapInt64X2 abs

    signum = mapInt64X2 signum

    fromInteger i =
        let !(I64# n#) = fromInteger i
            v#         = packInt64X2# n# n#
        in
          I64X2# v#

instance Show Int64X2 where
    showsPrec _ (I64X2# v#) =
        let !(# a#, b# #) = unpackInt64X2# v#
        in
          showString "<" . showv [I64# a#, I64# b#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Storable Int64X2 where
    sizeOf _     = 2*sIZEOF_INT64
    alignment _  = 2*sIZEOF_INT64
    peekElemOff  = readInt64X2OffPtr
    pokeElemOff  = writeInt64X2OffPtr

readInt64X2OffPtr :: Ptr Int64X2 -> Int -> IO Int64X2
readInt64X2OffPtr (Ptr a) (I# i) = IO $ \s ->
    case readInt64X2OffAddr# a i s of
      (# s2, x #) -> (# s2, I64X2# x #)

writeInt64X2OffPtr :: Ptr Int64X2 -> Int -> Int64X2 -> IO ()
writeInt64X2OffPtr (Ptr a) (I# i) (I64X2# x) = IO $ \s ->
    case writeInt64X2OffAddr# a i x s of
      s2 -> (# s2, () #)
#endif /* defined(__GLASGOW_HASKELL_LLVM__) */
