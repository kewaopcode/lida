{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}

module LuaJIT.Script where

import           Control.Applicative ((<|>))
import           Control.Lens.TH     (makePrisms)
import           Control.Monad       (replicateM, void)
import           Control.Monad.Loops (untilM)
import           Data.Bits           (clearBit, setBit, shiftL, shiftR, testBit,
                                      (.&.), (.|.))
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Int            (Int64)
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as M
import           Data.Serialize
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word           (Word16, Word32, Word64, Word8)
import qualified GHC.Float           as Float

import           LuaJIT.Instruction  (Instruction)
import           LuaJIT.TH           (makeLenses_)
import qualified LuaJIT.ULEB128      as ULEB128


type TolerantInstruction = Either ByteString Instruction


data LuaNumber
    = LuaDouble Double
    | LuaInt Word32
    deriving (Show, Eq)

makePrisms ''LuaNumber


data TableElement
    = KTAB_FALSE
    | KTAB_TRUE
    | KTAB_NIL
    | KTAB_STR ByteString
    | KTAB_INT Word32
    | KTAB_NUM Double
    deriving (Eq, Show, Ord)

makePrisms ''TableElement


data KGCTable
    = KGCArray (Vector TableElement)
    | KGCTable (Map TableElement TableElement)
    | KGCTableEmpty
    deriving (Eq, Show)

makeLenses_ ''KGCTable


data KGC
    = KGC_CHILD
    | KGC_TAB KGCTable
    | KGC_I64 Int64
    | KGC_U64 Word64
    | KGC_COMPLEX Word32 Word32 Word32 Word32
    | KGC_STR ByteString
    deriving (Eq, Show)

makePrisms ''KGC


data HeaderFlags = HeaderFlags
    { headerFlagsBE    :: Bool
    , headerFlagsStrip :: Bool
    , headerFlagsFFI   :: Bool
    , headerFlagsFR2   :: Bool
    } deriving (Eq, Show)

makeLenses_ ''HeaderFlags

data Proto = Proto
    { protoFlags        :: Word8
    , protoNumParams    :: Word8
    , protoFrameSize    :: Word8
    , protoFirstLine    :: Maybe Word32
    , protoNumLine      :: Maybe Word32
    , protoInstructions :: [TolerantInstruction]
    , protoUV           :: Vector Word16
    , protoConstantsGC  :: Vector KGC
    , protoConstantsNum :: Vector LuaNumber
    , protoDebug        :: Maybe ByteString
    } deriving (Eq, Show)

makeLenses_ ''Proto


data Header = Header
    { headerVersion   :: Word8
    , headerFlags     :: HeaderFlags
    , headerChunkName :: Maybe ByteString
    } deriving (Eq, Show)

makeLenses_ ''Header


data Script = Script
    { scriptHeader :: Header
    , scriptProtos :: [Proto]
    } deriving (Eq, Show)

makeLenses_ ''Script


decodeScript :: Get Script
decodeScript = do
    magic <- V.replicateM 3 getWord8

    if magic == V.fromList [ 0x1B, 0x4C, 0x4A ] then do
        header <- decodeHeader
        protos <- decodeProto (headerFlags header) `untilM` do b <- lookAhead getWord8
                                                               return $ b == 0x00

        return $ Script header protos
    else
        fail "wrong magic signature"


decodeHeader :: Get Header
decodeHeader = do
    version <- getWord8
    flags <- decodeHeaderFlags
    mbChunkName <- decodeIf (headerFlagsStrip flags == False) decodeChunkName

    return $ Header version flags mbChunkName


decodeHeaderFlags :: Get HeaderFlags
decodeHeaderFlags = do
    raw <- getWord8

    return $ HeaderFlags { headerFlagsBE = testBit raw 0
                         , headerFlagsStrip = testBit raw 1
                         , headerFlagsFFI = testBit raw 2
                         , headerFlagsFR2 = testBit raw 3
                         }


decodeChunkName :: Get ByteString
decodeChunkName = do
    len <- ULEB128.decode @Word32
    name <- getByteString $ fromIntegral len
    return name



decodeProto :: HeaderFlags -> Get Proto
decodeProto HeaderFlags{..} = do
    _ <- ULEB128.decode @Word32
    protoFlags <- getWord8
    protoNumParams <- getWord8
    protoFrameSize <- getWord8

    protoSizeUV :: Word8 <- getWord8
    protoSizeKGC :: Word32 <- ULEB128.decode
    protoSizeKN :: Word32 <- ULEB128.decode
    protoSizeBC :: Word32 <- ULEB128.decode

    protoSizeDbg :: Maybe Word32 <- decodeIf (headerFlagsStrip == False) (ULEB128.decode)
    protoFirstLine <- decodeIf (protoSizeDbg /= Just 0 && protoSizeDbg /= Nothing) ULEB128.decode
    protoNumLine <- decodeIf (protoSizeDbg /= Just 0 && protoSizeDbg /= Nothing) ULEB128.decode

    protoInstructions <- replicateM (fromIntegral protoSizeBC) decodeTolerantInstruction
    protoUV <- V.replicateM (fromIntegral protoSizeUV) getWord16host
    protoConstantsGC <- V.replicateM (fromIntegral protoSizeKGC) decodeKGC
    protoConstantsNum <- V.replicateM (fromIntegral protoSizeKN) decodeKNum
    protoDebug <- case protoSizeDbg of
                    Just size -> Just <$> getByteString (fromIntegral size)
                    Nothing   -> return Nothing

    return $ Proto {..}


decodeKGCTable :: Get KGCTable
decodeKGCTable = do
    arrayLen <- ULEB128.decode @Word32
    hashLen <- ULEB128.decode @Word32

    array <- V.replicateM (fromIntegral arrayLen) decodeTableElement
    table <- replicateM (fromIntegral hashLen) (getTwoOf decodeTableElement decodeTableElement)

    if | not (V.null array) && not (null table) -> fail "What the fuck"
       | not (V.null array)                     -> return $ KGCArray array
       | not (null table)                       -> return $ KGCTable $ M.fromList table
       | V.null array && null table             -> return KGCTableEmpty


decodeTableElement :: Get TableElement
decodeTableElement = do
    type' <- ULEB128.decode @Word32

    case type' of
        0 -> return KTAB_NIL
        1 -> return KTAB_FALSE
        2 -> return KTAB_TRUE
        3 -> KTAB_INT <$> ULEB128.decode
        4 -> (\(a, b) -> KTAB_NUM $ Float.castWord64ToDouble $ wordsTo64Bit b a) <$> getTwoOf ULEB128.decode ULEB128.decode
        _ -> KTAB_STR <$> getByteString (fromIntegral type' - 5)


decodeKGC :: Get KGC
decodeKGC = do
    type' <- ULEB128.decode @Word32

    case type' of
        0 -> return KGC_CHILD
        1 -> KGC_TAB <$> decodeKGCTable
        2 -> KGC_I64 <$> getInt64le
        3 -> KGC_U64 <$> getWord64le
        4 -> KGC_COMPLEX <$> ULEB128.decode <*> ULEB128.decode <*> ULEB128.decode <*> ULEB128.decode
        _ -> KGC_STR <$> getByteString (fromIntegral type' - 5)


decodeKNum :: Get LuaNumber
decodeKNum = do
    isnum <- (\b -> b .&. (1 :: Word8)) <$> lookAhead getWord8
    low <- ULEB128.decode33

    if isnum == 1 then do
        high <- ULEB128.decode
        return $ LuaDouble $ Float.castWord64ToDouble $ wordsTo64Bit high low
    else
        return $ LuaInt low


-- encoders

encodeScript :: Putter Script
encodeScript Script {..} = do
    putWord8 0x1B
    putWord8 0x4C
    putWord8 0x4A
    encodeHeader scriptHeader
    mapM_ (encodeWithSize . encodeProto) scriptProtos
    putWord8 0x00


encodeHeader :: Putter Header
encodeHeader Header {..} = do
    putWord8 headerVersion
    encodeHeaderFlags headerFlags
    encodeMaybe encodeChunkName headerChunkName


encodeHeaderFlags :: Putter HeaderFlags
encodeHeaderFlags HeaderFlags {..} = putWord8 $ apply (0 :: Word8)
    where
        setBit' ix True a  = setBit a ix
        setBit' ix False a = clearBit a ix

        apply = foldr1 (.) [ setBit' 0 headerFlagsBE
                           , setBit' 1 headerFlagsStrip
                           , setBit' 2 headerFlagsFFI
                           , setBit' 3 headerFlagsFR2
                           ]


encodeChunkName :: Putter ByteString
encodeChunkName bs = do
    ULEB128.encode @Word32 (fromIntegral $ BS.length bs)
    putByteString bs


encodeProto :: Putter Proto
encodeProto Proto {..} = do
    putWord8 protoFlags
    putWord8 protoNumParams
    putWord8 protoFrameSize
    putWord8 (fromIntegral $ V.length protoUV)
    ULEB128.encode @Word32 (fromIntegral $ length protoConstantsGC)
    ULEB128.encode @Word32 (fromIntegral $ length protoConstantsNum)
    ULEB128.encode @Word32 (fromIntegral $ length protoInstructions)

    encodeMaybe ULEB128.encode (BS.length <$> protoDebug)
    encodeMaybe ULEB128.encode protoFirstLine
    encodeMaybe ULEB128.encode protoNumLine

    mapM_ encodeTolerantInstruction protoInstructions
    mapM_ putWord16host protoUV
    mapM_ encodeKGC protoConstantsGC
    mapM_ encodeKNum protoConstantsNum
    encodeMaybe putByteString protoDebug


encodeKGCTable :: Putter KGCTable
encodeKGCTable t = do
    case t of
        KGCTable table -> do
            ULEB128.encode @Word8 0
            ULEB128.encode $ length table

            void $ M.traverseWithKey (curry $ putTwoOf encodeTableElement encodeTableElement) table

        KGCArray array -> do
            ULEB128.encode $ length array
            ULEB128.encode @Word8 0

            mapM_ encodeTableElement array

        KGCTableEmpty -> do
            ULEB128.encode @Word8 0
            ULEB128.encode @Word8 0



encodeTableElement :: Putter TableElement
encodeTableElement ktabk =
    case ktabk of
        KTAB_NIL         -> ULEB128.encode @Word32 0
        KTAB_FALSE       -> ULEB128.encode @Word32 1
        KTAB_TRUE        -> ULEB128.encode @Word32 2
        KTAB_INT uleb128 -> ULEB128.encode @Word32 3 >> ULEB128.encode uleb128
        KTAB_NUM n       -> do ULEB128.encode @Word32 4
                               putTwoOf ULEB128.encode ULEB128.encode (sepWord64 $ Float.castDoubleToWord64 n)
        KTAB_STR str     -> ULEB128.encode @Word32 (fromIntegral $ BS.length str + 5) >> putByteString str


encodeKGC :: Putter KGC
encodeKGC kgc =
    case kgc of
        KGC_CHILD           -> ULEB128.encode @Word32 0
        KGC_TAB tab         -> ULEB128.encode @Word32 1 >> encodeKGCTable tab
        KGC_I64 i64         -> ULEB128.encode @Word32 2 >> putInt64le i64
        KGC_U64 u64         -> ULEB128.encode @Word32 3 >> putWord64le u64
        KGC_COMPLEX a b c d -> ULEB128.encode @Word32 4 >> mapM_ ULEB128.encode [a, b, c, d]
        KGC_STR str         -> ULEB128.encode @Word32 (fromIntegral $ BS.length str + 5) >> putByteString str


encodeKNum :: Putter LuaNumber
encodeKNum luaNumber =
    case luaNumber of
        LuaInt i ->
            ULEB128.encode33 False $ fromIntegral i

        LuaDouble d -> do
            let (f, s) = sepWord64 $ Float.castDoubleToWord64 d
            ULEB128.encode33 True $ fromIntegral f
            ULEB128.encode s


-- helpers


decodeIf :: Bool -> Get a -> Get (Maybe a)
decodeIf condition decoder =
    if condition then
        Just <$> decoder

    else
        return Nothing


encodeMaybe :: Putter a -> Putter (Maybe a)
encodeMaybe _ Nothing       = return ()
encodeMaybe putter (Just a) = putter a


decodeTolerantInstruction :: Get TolerantInstruction
decodeTolerantInstruction =
    Right <$> get @Instruction <|> Left <$> getByteString 4


encodeTolerantInstruction :: Putter TolerantInstruction
encodeTolerantInstruction (Left bs) = putByteString bs
encodeTolerantInstruction (Right i) = put @Instruction i


encodeWithSize :: Put -> Put
encodeWithSize p = do
    let bs = runPut p
    ULEB128.encode $ fromIntegral @Int @Word32 $ BS.length bs
    putByteString bs


wordsTo64Bit :: Word32 -> Word32 -> Word64
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)


sepWord64 :: Word64 -> (Word32, Word32)
sepWord64 w64 =
    ( fromIntegral w64
    , fromIntegral $ w64 `shiftR` 32
    )
