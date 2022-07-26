{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Lida.XRef where

import           Control.Lens       (imap, (&))
import           Data.Map.Lazy      (Map)
import qualified Data.Map.Lazy      as Map
import           Data.Maybe         (catMaybes)
import           Data.Word          (Word16, Word8)
import           Generics.SOP

import           LuaJIT.Instruction
import           LuaJIT.Script


data Navigation
    = NavProto Int Navigation
    | NavConstantGC Int
    | NavConstantNum Int
    | NavInstruction Int
    | NavEnd
    deriving (Eq, Ord, Show)


data ReferenceType
    = RefBase
    | RefRBase
    | RefStr
    | RefTab
    | RefFunc
    | RefCData
    | RefJump
    deriving Show

data Reference = Reference ReferenceType Navigation
    deriving Show

-- | Gets absoulute reference for an operand
class OperandReference a where
    operandReference :: Int -> Int -> a -> Maybe Reference

instance Integral a => OperandReference (None a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Var a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Dst a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Base a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (RBase a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (UV a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Lit a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Lits a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Num' a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (Pri a) where operandReference _ _ _ = Nothing
instance Integral a => OperandReference (CData a) where operandReference _ _ _ = Nothing

instance Integral a => OperandReference (Str a) where
    operandReference baseProto _ (Str a) =
        Just $ Reference RefStr (NavProto baseProto $ NavConstantGC $ fromIntegral a)

instance Integral a => OperandReference (Tab a) where
    operandReference baseProto _ (Tab a) =
        Just $ Reference RefTab (NavProto baseProto $ NavConstantGC $ fromIntegral a)

instance Integral a => OperandReference (Func a) where
    operandReference _ _ (Func a) =
        Just $ Reference RefFunc (NavProto (fromIntegral a) NavEnd)

instance Integral a => OperandReference (Jump a) where
    operandReference baseProto baseIns (Jump a) =
        Just $ Reference RefJump (NavProto baseProto $ NavInstruction (calcJump baseIns (fromIntegral a)))


class OperandsReferences a where
    operandsReferences :: Int -> Int -> a -> [Reference]

instance (All OperandReference [a Word8, b Word8, c Word8]) => OperandsReferences (OpABC a b c) where
    operandsReferences baseProto baseIns (OpABC a b c) =
        catMaybes [ operandReference baseProto baseIns a
                  , operandReference baseProto baseIns b
                  , operandReference baseProto baseIns c
                  ]

instance (All OperandReference [a Word8, d Word16]) => OperandsReferences (OpAD a d) where
    operandsReferences baseProto baseIns (OpAD a d) =
        catMaybes [ operandReference baseProto baseIns a
                  , operandReference baseProto baseIns d
                  ]


instructionReferences :: Int -> Int -> Instruction -> [Reference]
instructionReferences baseProto baseIns instruction =
    hcollapse $ hcmap (Proxy @(All OperandsReferences))
                      (\(o :* Nil) -> mapIK (operandsReferences baseProto baseIns) o)
                      (unSOP $ from instruction)


crossReferences :: Script -> Map Navigation [Reference]
crossReferences Script{..} =
    let
        mkRef :: Navigation -> [Reference] -> Map Navigation [Reference]
        mkRef nav refs =
            foldr (\(Reference t nav') -> Map.insert nav' [Reference t nav]) Map.empty refs


        handleOne :: Int -> Proto -> Map Navigation [Reference]
        handleOne baseProto Proto{..} =
            protoInstructions
                & imap
                    (\ix ins -> case ins of
                        Right i -> mkRef (NavProto baseProto $ NavInstruction ix) $ instructionReferences baseProto ix i
                        Left _ -> Map.empty
                    )
                & Map.unionsWith (++)
    in
    Map.unionsWith (++) $ imap handleOne scriptProtos


-- * Helpers

calcJump :: Int -> Int -> Int
calcJump base jump = base + jump - 0x8000 + 1
