{-| generics-sop's compile-time performance is bad, you need some extra time to compile this module.
    So i decided to move it into its own module.
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module LuaJIT.Instruction where

import           Control.DeepSeq
import           Control.Lens.TH     (makePrisms)
import           Data.SOP.Constraint
import           Data.Serialize
import           Data.Vector         ((!?))
import qualified Data.Vector         as V
import           Data.Word           (Word16, Word8)
import qualified GHC.Generics        as GHC
import           Generics.SOP
import           Generics.SOP.TH


instance IsNewtype (op Word8) Word8 => Serialize (op Word8) where
    get = newtypeTo <$> getWord8
    put a = putWord8 $ newtypeFrom a

instance IsNewtype (op Word16) Word16 => Serialize (op Word16) where
    get = newtypeTo <$> getWord16le
    put a = putWord16host $ newtypeFrom a


newtype None    data' = None    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Var     data' = Var     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Dst     data' = Dst     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Base    data' = Base    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype RBase   data' = RBase   data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype UV      data' = UV      data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Lit     data' = Lit     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Lits    data' = Lits    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Num'    data' = Num'    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Str     data' = Str     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Tab     data' = Tab     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Func    data' = Func    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype CData   data' = CData   data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Jump    data' = Jump    data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData
newtype Pri     data' = Pri     data' deriving (Eq, Ord, Show, Read, GHC.Generic) deriving anyclass NFData


deriveGeneric ''None
deriveGeneric ''Var
deriveGeneric ''Dst
deriveGeneric ''Base
deriveGeneric ''RBase
deriveGeneric ''UV
deriveGeneric ''Lit
deriveGeneric ''Lits
deriveGeneric ''Num'
deriveGeneric ''Str
deriveGeneric ''Tab
deriveGeneric ''Func
deriveGeneric ''CData
deriveGeneric ''Jump
deriveGeneric ''Pri


data OpABC a b c = OpABC !(a Word8) !(b Word8) !(c Word8)
deriving instance (All Show [a Word8, b Word8, c Word8]) => Show (OpABC a b c)
deriving instance (All GHC.Generic [a Word8, b Word8, c Word8]) => GHC.Generic (OpABC a b c)
deriving instance (All Eq [a Word8, b Word8, c Word8]) => Eq (OpABC a b c)
deriving instance (All Ord [a Word8, b Word8, c Word8]) => Ord (OpABC a b c)
deriving instance (All (And NFData GHC.Generic) [a Word8, b Word8, c Word8]) => NFData (OpABC a b c)
instance (All Serialize [a Word8, b Word8, c Word8]) => Serialize (OpABC a b c) where
    get = (\a c b -> OpABC a b c) <$> get <*> get <*> get
    put (OpABC a b c) = put a >> put c >> put b

deriveGeneric ''OpABC


data OpAD a d = OpAD !(a Word8) !(d Word16)
deriving instance (All Show [a Word8, d Word16]) => Show (OpAD a d)
deriving instance (All GHC.Generic [a Word8, d Word16]) => GHC.Generic (OpAD a d)
deriving instance (All Eq [a Word8, d Word16]) => Eq (OpAD a d)
deriving instance (All Ord [a Word8, d Word16]) => Ord (OpAD a d)
deriving instance (All (And NFData GHC.Generic) [a Word8, d Word16]) => NFData (OpAD a d)
instance (All Serialize [a Word8, d Word16]) => Serialize (OpAD a d) where
    get = OpAD <$> get <*> get
    put (OpAD a d) = put a >> put d

deriveGeneric ''OpAD


data Instruction
    = ISLT      {-# UNPACK #-} !(OpAD Var Var)
    | ISGE      {-# UNPACK #-} !(OpAD Var Var)
    | ISLE      {-# UNPACK #-} !(OpAD Var Var)
    | ISGT      {-# UNPACK #-} !(OpAD Var Var)
    | ISEQV     {-# UNPACK #-} !(OpAD Var Var)
    | ISNEV     {-# UNPACK #-} !(OpAD Var Var)
    | ISEQS     {-# UNPACK #-} !(OpAD Var Str)
    | ISNES     {-# UNPACK #-} !(OpAD Var Str)
    | ISEQN     {-# UNPACK #-} !(OpAD Var Num')
    | ISNEN     {-# UNPACK #-} !(OpAD Var Num')
    | ISEQP     {-# UNPACK #-} !(OpAD Var Pri)
    | ISNEP     {-# UNPACK #-} !(OpAD Var Pri)
    | ISTC      {-# UNPACK #-} !(OpAD Dst Var)
    | ISFC      {-# UNPACK #-} !(OpAD Dst Var)
    | IST       {-# UNPACK #-} !(OpAD None Var)
    | ISF       {-# UNPACK #-} !(OpAD None Var)
    | ISTYPE    {-# UNPACK #-} !(OpAD Var Lit)
    | ISNUM     {-# UNPACK #-} !(OpAD Var Lit)
    | MOV       {-# UNPACK #-} !(OpAD Dst Var)
    | NOT       {-# UNPACK #-} !(OpAD Dst Var)
    | UNM       {-# UNPACK #-} !(OpAD Dst Var)
    | LEN       {-# UNPACK #-} !(OpAD Dst Var)
    | ADDVN     {-# UNPACK #-} !(OpABC Dst Var Num')
    | SUBVN     {-# UNPACK #-} !(OpABC Dst Var Num')
    | MULVN     {-# UNPACK #-} !(OpABC Dst Var Num')
    | DIVVN     {-# UNPACK #-} !(OpABC Dst Var Num')
    | MODVN     {-# UNPACK #-} !(OpABC Dst Var Num')
    | ADDNV     {-# UNPACK #-} !(OpABC Dst Var Num')
    | SUBNV     {-# UNPACK #-} !(OpABC Dst Var Num')
    | MULNV     {-# UNPACK #-} !(OpABC Dst Var Num')
    | DIVNV     {-# UNPACK #-} !(OpABC Dst Var Num')
    | MODNV     {-# UNPACK #-} !(OpABC Dst Var Num')
    | ADDVV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | SUBVV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | MULVV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | DIVVV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | MODVV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | POW       {-# UNPACK #-} !(OpABC Dst Var Var)
    | CAT       {-# UNPACK #-} !(OpABC Dst RBase RBase)
    | KSTR      {-# UNPACK #-} !(OpAD Dst Str)
    | KCDATA    {-# UNPACK #-} !(OpAD Dst CData)
    | KSHORT    {-# UNPACK #-} !(OpAD Dst Lits)
    | KNUM      {-# UNPACK #-} !(OpAD Dst Num')
    | KPRI      {-# UNPACK #-} !(OpAD Dst Pri)
    | KNIL      {-# UNPACK #-} !(OpAD Base Base)
    | UGET      {-# UNPACK #-} !(OpAD Dst UV)
    | USETV     {-# UNPACK #-} !(OpAD UV Var)
    | USETS     {-# UNPACK #-} !(OpAD UV Str)
    | USETN     {-# UNPACK #-} !(OpAD UV Num')
    | USETP     {-# UNPACK #-} !(OpAD UV Pri)
    | UCLO      {-# UNPACK #-} !(OpAD RBase Jump)
    | FNEW      {-# UNPACK #-} !(OpAD Dst Func)
    | TNEW      {-# UNPACK #-} !(OpAD Dst Lit)
    | TDUP      {-# UNPACK #-} !(OpAD Dst Tab)
    | GGET      {-# UNPACK #-} !(OpAD Dst Str)
    | GSET      {-# UNPACK #-} !(OpAD Var Str)
    | TGETV     {-# UNPACK #-} !(OpABC Dst Var Var)
    | TGETS     {-# UNPACK #-} !(OpABC Dst Var Str)
    | TGETB     {-# UNPACK #-} !(OpABC Dst Var Lit)
    | TGETR     {-# UNPACK #-} !(OpABC Dst Var Var)
    | TSETV     {-# UNPACK #-} !(OpABC Var Var Var)
    | TSETS     {-# UNPACK #-} !(OpABC Var Var Str)
    | TSETB     {-# UNPACK #-} !(OpABC Var Var Lit)
    | TSETM     {-# UNPACK #-} !(OpAD Base Num')
    | TSETR     {-# UNPACK #-} !(OpABC Var Var Var)
    | CALLM     {-# UNPACK #-} !(OpABC Base Lit Lit)
    | CALL      {-# UNPACK #-} !(OpABC Base Lit Lit)
    | CALLMT    {-# UNPACK #-} !(OpAD Base Lit)
    | CALLT     {-# UNPACK #-} !(OpAD Base Lit)
    | ITERC     {-# UNPACK #-} !(OpABC Base Lit Lit)
    | ITERN     {-# UNPACK #-} !(OpABC Base Lit Lit)
    | VARG      {-# UNPACK #-} !(OpABC Base Lit Lit)
    | ISNEXT    {-# UNPACK #-} !(OpAD Base Jump)
    | RETM      {-# UNPACK #-} !(OpAD Base Lit)
    | RET       {-# UNPACK #-} !(OpAD RBase Lit)
    | RET0      {-# UNPACK #-} !(OpAD RBase Lit)
    | RET1      {-# UNPACK #-} !(OpAD RBase Lit)
    | FORI      {-# UNPACK #-} !(OpAD Base Jump)
    | JFORI     {-# UNPACK #-} !(OpAD Base Jump)
    | FORL      {-# UNPACK #-} !(OpAD Base Jump)
    | IFORL     {-# UNPACK #-} !(OpAD Base Jump)
    | JFORL     {-# UNPACK #-} !(OpAD Base Lit)
    | ITERL     {-# UNPACK #-} !(OpAD Base Jump)
    | IITERL    {-# UNPACK #-} !(OpAD Base Jump)
    | JITERL    {-# UNPACK #-} !(OpAD Base Lit)
    | LOOP      {-# UNPACK #-} !(OpAD RBase Jump)
    | ILOOP     {-# UNPACK #-} !(OpAD RBase Jump)
    | JLOOP     {-# UNPACK #-} !(OpAD RBase Lit)
    | JMP       {-# UNPACK #-} !(OpAD RBase Jump)
    | FUNCF     {-# UNPACK #-} !(OpAD RBase None)
    | IFUNCF    {-# UNPACK #-} !(OpAD RBase None)
    | JFUNCF    {-# UNPACK #-} !(OpAD RBase Lit)
    | FUNCV     {-# UNPACK #-} !(OpAD RBase None)
    | IFUNCV    {-# UNPACK #-} !(OpAD RBase None)
    | JFUNCV    {-# UNPACK #-} !(OpAD RBase Lit)
    | FUNCC     {-# UNPACK #-} !(OpAD RBase None)
    | FUNCCW    {-# UNPACK #-} !(OpAD RBase None)
    deriving (Eq, Ord, Show, GHC.Generic, NFData)

deriveGeneric ''Instruction
makePrisms ''Instruction


instance Serialize Instruction where
    put instruction = do
        let sop = from instruction

        putWord8 $ fromIntegral (hindex sop)
        hctraverse_ (Proxy @(All Serialize)) (\(I params :* Nil) -> put params) (unSOP sop)

    get = do
        let opcodes = V.fromList $ hapInjs injections
        mbInj <- (\o -> opcodes !? fromIntegral o) <$> getWord8

        case mbInj of
            Just inj -> hcollapse $ hcmap (Proxy @GetInstruction) (K . getInstruction) inj
            Nothing  -> fail "undefined opcode"


class GetInstruction xs where
  getInstruction :: Injection (NP I) (Code Instruction) xs -> Get Instruction

instance (Serialize params) => GetInstruction '[params] where
  getInstruction (Fn inj) = do
    params <- get
    return $ to $ SOP $ unK $ inj (I params :* Nil)
