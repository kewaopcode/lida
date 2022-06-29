{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module LuaJIT.ULEB128 where

import           Data.Bits      (Bits, clearBit, complement, shiftR, testBit,
                                 unsafeShiftL, (.&.), (.|.))
import           Data.Serialize (Get, Put)
import qualified Data.Serialize as C
import           Data.Word      (Word8)


decode33 :: (Bits a, Integral a) => Get a
decode33 = do
    v <- flip shiftR 1 <$> C.getWord8

    if v >= 0x40 then
        go 6 (fromIntegral (v .&. 0x3f))
    else
        return $ fromIntegral v
    where
        go shift w = do
            b <- C.getWord8
            let val = w .|. ((fromIntegral b .&. 0x7f) `unsafeShiftL` shift)
            if b >= 0x80
            then go (shift+7) val
            else return val


encode33 :: Bool -> Integer -> Put
encode33 isNum val = do
    let n = (1 + 2 * val)
    encode $ if isNum then n .|. 1 else n .&. complement 1


encode :: (Num a, Bits a, Integral a) => a -> Put
encode a =
    let
        bytes :: forall x. (Num x, Bits x, Integral x) => x -> [Word8]
        bytes x =
            if x >= 0x80 then
                (fromIntegral ((x .&. 0x7F) .|. 0x80)) : bytes (shiftR x 7)
            else
                pure $ fromIntegral x
    in
    mapM_ C.put $ bytes a


decode :: (Num a, Bits a) => Get a
decode = go 0 0
    where
        go shift w = do
            b <- C.getWord8
            let hasMore = testBit b 7
            let val = w .|. ((clearBit (fromIntegral b) 7) `unsafeShiftL` shift)
            if hasMore then
                go (shift + 7) val
            else
                return val
