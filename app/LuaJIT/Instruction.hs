{-| generics-sop's compile-time performance is bad, you need some extra time to compile this module.
    So i decided to move it into its own module.
-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module LuaJIT.Instruction where

import           Control.Lens.TH     (makePrisms)
import           Data.SOP.Constraint
import           Data.Serialize
import           Data.Vector         ((!?))
import qualified Data.Vector         as V
import           Data.Word           (Word16, Word8)
import           Generics.SOP
import           Generics.SOP.TH


instance IsNewtype (op Word8) Word8 => Serialize (op Word8) where
    get = newtypeTo <$> getWord8
    put a = putWord8 $ newtypeFrom a


instance IsNewtype (op Word16) Word16 => Serialize (op Word16) where
    get = newtypeTo <$> getWord16host
    put a = putWord16host $ newtypeFrom a


newtype None    data' = None    data' deriving (Eq, Ord, Show, Read)
newtype Var     data' = Var     data' deriving (Eq, Ord, Show, Read)
newtype Dst     data' = Dst     data' deriving (Eq, Ord, Show, Read)
newtype Base    data' = Base    data' deriving (Eq, Ord, Show, Read)
newtype RBase   data' = RBase   data' deriving (Eq, Ord, Show, Read)
newtype UV      data' = UV      data' deriving (Eq, Ord, Show, Read)
newtype Lit     data' = Lit     data' deriving (Eq, Ord, Show, Read)
newtype Lits    data' = Lits    data' deriving (Eq, Ord, Show, Read)
newtype Num'    data' = Num'    data' deriving (Eq, Ord, Show, Read)
newtype Str     data' = Str     data' deriving (Eq, Ord, Show, Read)
newtype Tab     data' = Tab     data' deriving (Eq, Ord, Show, Read)
newtype Func    data' = Func    data' deriving (Eq, Ord, Show, Read)
newtype CData   data' = CData   data' deriving (Eq, Ord, Show, Read)
newtype Jump    data' = Jump    data' deriving (Eq, Ord, Show, Read)
newtype Pri     data' = Pri     data' deriving (Eq, Ord, Show, Read)

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


data OpABC a b c = OpABC (a Word8) (b Word8) (c Word8)
deriving instance (All Show [a Word8, b Word8, c Word8]) => Show (OpABC a b c)
deriving instance (All Read [a Word8, b Word8, c Word8]) => Read (OpABC a b c)
deriving instance (All Eq [a Word8, b Word8, c Word8]) => Eq (OpABC a b c)
deriving instance (All Ord [a Word8, b Word8, c Word8]) => Ord (OpABC a b c)
instance (All Serialize [a Word8, b Word8, c Word8]) => Serialize (OpABC a b c) where
    get = (\a c b -> OpABC a b c) <$> get <*> get <*> get
    put (OpABC a b c) = put a >> put c >> put b

deriveGeneric ''OpABC


data OpAD a d = OpAD (a Word8) (d Word16)
deriving instance (All Show [a Word8, d Word16]) => Show (OpAD a d)
deriving instance (All Read [a Word8, d Word16]) => Read (OpAD a d)
deriving instance (All Eq [a Word8, d Word16]) => Eq (OpAD a d)
deriving instance (All Ord [a Word8, d Word16]) => Ord (OpAD a d)
instance (All Serialize [a Word8, d Word16]) => Serialize (OpAD a d) where
    get = OpAD <$> get <*> get
    put (OpAD a d) = put a >> put d

deriveGeneric ''OpAD


data Instruction
    = ISLT      (OpAD Var Var)
    | ISGE      (OpAD Var Var)
    | ISLE      (OpAD Var Var)
    | ISGT      (OpAD Var Var)
    | ISEQV     (OpAD Var Var)
    | ISNEV     (OpAD Var Var)
    | ISEQS     (OpAD Var Str)
    | ISNES     (OpAD Var Str)
    | ISEQN     (OpAD Var Num')
    | ISNEN     (OpAD Var Num')
    | ISEQP     (OpAD Var Pri)
    | ISNEP     (OpAD Var Pri)
    | ISTC      (OpAD Dst Var)
    | ISFC      (OpAD Dst Var)
    | IST       (OpAD None Var)
    | ISF       (OpAD None Var)
    | ISTYPE    (OpAD Var Lit)
    | ISNUM     (OpAD Var Lit)
    | MOV       (OpAD Dst Var)
    | NOT       (OpAD Dst Var)
    | UNM       (OpAD Dst Var)
    | LEN       (OpAD Dst Var)
    | ADDVN     (OpABC Dst Var Num')
    | SUBVN     (OpABC Dst Var Num')
    | MULVN     (OpABC Dst Var Num')
    | DIVVN     (OpABC Dst Var Num')
    | MODVN     (OpABC Dst Var Num')
    | ADDNV     (OpABC Dst Var Num')
    | SUBNV     (OpABC Dst Var Num')
    | MULNV     (OpABC Dst Var Num')
    | DIVNV     (OpABC Dst Var Num')
    | MODNV     (OpABC Dst Var Num')
    | ADDVV     (OpABC Dst Var Var)
    | SUBVV     (OpABC Dst Var Var)
    | MULVV     (OpABC Dst Var Var)
    | DIVVV     (OpABC Dst Var Var)
    | MODVV     (OpABC Dst Var Var)
    | POW       (OpABC Dst Var Var)
    | CAT       (OpABC Dst RBase RBase)
    | KSTR      (OpAD Dst Str)
    | KCDATA    (OpAD Dst CData)
    | KSHORT    (OpAD Dst Lits)
    | KNUM      (OpAD Dst Num')
    | KPRI      (OpAD Dst Pri)
    | KNIL      (OpAD Base Base)
    | UGET      (OpAD Dst UV)
    | USETV     (OpAD UV Var)
    | USETS     (OpAD UV Str)
    | USETN     (OpAD UV Num')
    | USETP     (OpAD UV Pri)
    | UCLO      (OpAD RBase Jump)
    | FNEW      (OpAD Dst Func)
    | TNEW      (OpAD Dst Lit)
    | TDUP      (OpAD Dst Tab)
    | GGET      (OpAD Dst Str)
    | GSET      (OpAD Var Str)
    | TGETV     (OpABC Dst Var Var)
    | TGETS     (OpABC Dst Var Str)
    | TGETB     (OpABC Dst Var Lit)
    | TGETR     (OpABC Dst Var Var)
    | TSETV     (OpABC Var Var Var)
    | TSETS     (OpABC Var Var Str)
    | TSETB     (OpABC Var Var Lit)
    | TSETM     (OpAD Base Num')
    | TSETR     (OpABC Var Var Var)
    | CALLM     (OpABC Base Lit Lit)
    | CALL      (OpABC Base Lit Lit)
    | CALLMT    (OpAD Base Lit)
    | CALLT     (OpAD Base Lit)
    | ITERC     (OpABC Base Lit Lit)
    | ITERN     (OpABC Base Lit Lit)
    | VARG      (OpABC Base Lit Lit)
    | ISNEXT    (OpAD Base Jump)
    | RETM      (OpAD Base Lit)
    | RET       (OpAD RBase Lit)
    | RET0      (OpAD RBase Lit)
    | RET1      (OpAD RBase Lit)
    | FORI      (OpAD Base Jump)
    | JFORI     (OpAD Base Jump)
    | FORL      (OpAD Base Jump)
    | IFORL     (OpAD Base Jump)
    | JFORL     (OpAD Base Lit)
    | ITERL     (OpAD Base Jump)
    | IITERL    (OpAD Base Jump)
    | JITERL    (OpAD Base Lit)
    | LOOP      (OpAD RBase Jump)
    | ILOOP     (OpAD RBase Jump)
    | JLOOP     (OpAD RBase Lit)
    | JMP       (OpAD RBase Jump)
    | FUNCF     (OpAD RBase None)
    | IFUNCF    (OpAD RBase None)
    | JFUNCF    (OpAD RBase Lit)
    | FUNCV     (OpAD RBase None)
    | IFUNCV    (OpAD RBase None)
    | JFUNCV    (OpAD RBase Lit)
    | FUNCC     (OpAD RBase None)
    | FUNCCW    (OpAD RBase None)
    deriving (Eq, Ord, Show, Read)

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

        maybe
            (fail "undefined opcode")
            (hcollapse . hcmap (Proxy @GetInstruction) (K . getInstruction))
            mbInj


class GetInstruction xs where
  getInstruction :: Injection (NP I) (Code Instruction) xs -> Get Instruction

instance (Serialize params) => GetInstruction '[params] where
  getInstruction (Fn inj) = do
    params <- get
    return $ to $ SOP $ unK $ inj (I params :* Nil)
