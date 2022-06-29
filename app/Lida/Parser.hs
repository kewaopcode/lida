{-| Parser of statements from instructions
-}

{-# LANGUAGE RankNTypes #-}
module Lida.Parser where

import           Control.Lens
import           Control.Monad.Loops  (untilM)
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import           Data.Either
import           Data.Foldable        (asum)
import qualified Data.Serialize       as Cereal
import qualified Data.Set             as Set
import           Data.Void            (Void)
import           Data.Word            (Word16, Word8)
import           Text.Megaparsec      as MP

import           LuaJIT.Instruction
import           LuaJIT.Script


data Statement
    = RawInstruction Instruction
    | NumericFor Word8 Word8 Word8 [Statement]
    | NumericForAnalyzed Word16 Word16 Word16 [Statement]
    | RepeatUntil [Statement]
    | IfThenElse [Statement] [Statement]
    deriving Show


type Parser a = ParsecT Void [Instruction] (Reader Proto) a


parser :: Parser [Statement]
parser = many parseStatement


parseStatement :: Parser Statement
parseStatement =
    asum [ try repeatUntil
         , try numericForAnalyzed
         , try numericFor
         , try rawInstruction
         ]


numericFor :: Parser Statement
numericFor = do
    OpAD (Base base) (Jump _) <- instruction _FORI
    body <- parseStatement `untilM` (isRight <$> observing (instruction _FORL))
    return $ NumericFor base (base + 1) (base + 2) body


numericForAnalyzed :: Parser Statement
numericForAnalyzed = do
    OpAD _ (Lits valA) <- instruction _KSHORT
    OpAD _ (Lits valB) <- instruction _KSHORT
    OpAD _ (Lits valC) <- instruction _KSHORT
    OpAD _ (Jump _) <- instruction _FORI
    body <- parseStatement `untilM` (isRight <$> observing (instruction _FORL))
    return $ NumericForAnalyzed valA valB valC body


repeatUntil :: Parser Statement
repeatUntil = do
    OpAD {} <- instruction _LOOP
    body <- parseStatement `untilM` (isRight <$> observing (lookAhead $ instruction _JMP))
    return $ RepeatUntil body


rawInstruction :: Parser Statement
rawInstruction = token (Just . RawInstruction) Set.empty


-- helpers

instruction :: Prism' Instruction op -> Parser op
instruction p = token (\i -> i ^? p) Set.empty
