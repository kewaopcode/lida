{-| Parser of statements from instructions
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Lida.Parser where

import           Control.Lens
import           Control.Monad.Loops  (untilM)
import           Control.Monad.Reader
import qualified Data.Either          as Either
import           Data.Foldable        (asum)
import qualified Data.Set             as Set
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Data.Word            (Word8)
import           Text.Megaparsec      as MP

import           LuaJIT.Instruction
import           LuaJIT.Script


data Statement
    = RawInstruction Instruction
    | NumericFor Word8 Word8 Word8 [Statement]
    | RepeatUntil (Maybe Instruction) [Statement] -- ^ the code is malformed in case of Nothing
    | IfThenElse (Maybe Instruction) [Statement] [Statement] -- ^ the code is malformed in case of Nothing
    deriving Show


type Parser a = ParsecT Void [Instruction] (Reader Proto) a


parse :: Proto -> Either (ParseErrorBundle [Instruction] Void) [Statement]
parse proto =
    runParserT parser "proto" (Either.rights $ V.toList $ protoInstructions proto)
        & flip runReader proto


parser :: Parser [Statement]
parser = many parseStatement


parseStatement :: Parser Statement
parseStatement =
    asum [ try repeatUntil
         , try numericFor
         , try anyInstruction
         ]


numericFor :: Parser Statement
numericFor = do
    OpAD (Base base) (Jump _) <- instruction _FORI
    body <- parseStatement `parseUntil` instruction _FORL
    return $ NumericFor base (base + 1) (base + 2) body


repeatUntil :: Parser Statement
repeatUntil = do
    OpAD {} <- instruction _LOOP
    body <- parseStatement `parseUntil` instruction _JMP
    let (RawInstruction cond) = last body

    if isCondition cond then
        return $ RepeatUntil (Just cond) (init body)
    else
        return $ RepeatUntil Nothing body


anyInstruction :: Parser Statement
anyInstruction = token (Just . RawInstruction) Set.empty


-- * Helpers

instruction :: Prism' Instruction op -> Parser op
instruction p = token (\i -> i ^? p) Set.empty


-- | Does not consume the succeed `that`
parseUntil :: Parser a -> Parser b -> Parser [a]
parseUntil p that =
    p `untilM` (Either.isRight <$> observing (lookAhead that))


isCondition :: Instruction -> Bool
isCondition = \case
    ISLT _  -> True
    ISGE _  -> True
    ISLE _  -> True
    ISGT _  -> True
    ISEQV _ -> True
    ISNEV _ -> True
    ISEQS _ -> True
    ISNES _ -> True
    ISEQN _ -> True
    ISNEN _ -> True
    ISEQP _ -> True
    ISNEP _ -> True
    ISTC _  -> True
    ISFC _  -> True
    IST _   -> True
    ISF _   -> True
    _       -> False
