-- TODO: Think about new name for this module

{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}
module Lida.InstructionText where

import           Control.Monad        (void)
import           Data.Function        ((&))
import           Data.Kind            (Type)
import           Data.Maybe           (catMaybes, listToMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word            (Word16, Word8)
import           Generics.SOP
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           LuaJIT.Instruction


-- example: "addvv 1 2 3"
parseInstruction :: Text -> Maybe Instruction
parseInstruction text =
    let
        constraint = Proxy @GParseInstruction
        constructors = constructorInfo $ datatypeInfo $ Proxy @Instruction
    in
    hcliftA2 constraint genericParser injections constructors
        & hcollapse
        & map (flip parseMaybe text)
        & catMaybes
        & listToMaybe


genericParser
    :: GParseInstruction a
    => Injection (NP I) (Code Instruction) a
    -> ConstructorInfo a
    -> K (Parsec Text Text Instruction) a
genericParser inj info = K do
    void $ string' $ T.pack (constructorName info)
    void $ space
    gparseInstruction inj


class GParseInstruction (a :: [Type]) where
    gparseInstruction :: Injection (NP I) (Code Instruction) a -> Parsec Text Text Instruction


instance
    ( IsNewtype (f Word8) Word8
    , IsNewtype (g Word8) Word8
    , IsNewtype (h Word8) Word8
    ) => GParseInstruction '[OpABC f g h] where
    gparseInstruction (Fn inj) = do
        fa <- newtypeTo . read <$> some digitChar
        void $ hspace1
        fb <- newtypeTo . read <$> some digitChar
        void $ hspace1
        fc <- newtypeTo . read <$> some digitChar
        return $ to $ SOP $ unK $ inj (I (OpABC fa fb fc) :* Nil)


instance
    ( IsNewtype (f Word8) Word8
    , IsNewtype (g Word16) Word16
    ) => GParseInstruction '[OpAD f g] where
    gparseInstruction (Fn inj) = do
        fa <- newtypeTo . read <$> some digitChar
        void $ hspace1
        fd <- newtypeTo . read <$> some digitChar
        return $ to $ SOP $ unK $ inj (I (OpAD fa fd) :* Nil)


class GShowInstruction (a :: [Type]) where
    gShowInstruction :: ConstructorInfo a -> NP I a -> K Text a


instance
    ( IsNewtype (f Word8) Word8
    , IsNewtype (g Word8) Word8
    , IsNewtype (h Word8) Word8
    ) => GShowInstruction '[OpABC f g h] where
    gShowInstruction constructor (I (OpABC a b c) :* Nil) =
        case constructor of
            Constructor cs ->
                K $ mconcat
                    [ T.pack cs
                    , " "
                    , T.pack $ show $ newtypeFrom a
                    , " "
                    , T.pack $ show $ newtypeFrom b
                    , " "
                    , T.pack $ show $ newtypeFrom c
                    ]
            _ ->
                error "invalid ConstructorInfo"


instance
    ( IsNewtype (f Word8) Word8
    , IsNewtype (g Word16) Word16
    ) => GShowInstruction '[OpAD f g] where
    gShowInstruction constructor (I (OpAD a d) :* Nil) =
        case constructor of
            Constructor cs ->
                K $ mconcat
                    [ T.pack cs
                    , " "
                    , T.pack $ show $ newtypeFrom a
                    , " "
                    , T.pack $ show $ newtypeFrom d
                    ]
            _ ->
                error "invalid ConstructorInfo"
