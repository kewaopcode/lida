module Main where

import qualified Data.ByteString     as BS
import qualified Data.Serialize      as Cereal
import qualified Options.Applicative as Opts
import qualified System.Environment  as Environment
import qualified Test.Tasty          as Tasty
--import qualified Text.Pretty.Simple  as Pretty

import qualified LuaJIT.Script       as Script
import qualified Test                as Test
import qualified Weigh

data Options = Options
    { scriptPath     :: FilePath
    , testIsomorphic :: Bool
    } deriving Show


options :: Opts.Parser Options
options = Options <$> filePath <*> iso where
    filePath =
        Opts.strArgument
             $ Opts.metavar "FILE"
            <> Opts.help "File you want to disassembly"

    iso =
        Opts.flag False True
             $ Opts.long "test-iso"
            <> Opts.help "Test if disassembler is isomorphic to assembler"


cliProg :: Opts.ParserInfo Options
cliProg =
    Opts.info (options Opts.<**> Opts.helper) (Opts.fullDesc <> Opts.header "LIDA - LuaJIT Interactive Disassembler")


main :: IO ()
main = do
    Options{..} <- Opts.customExecParser (Opts.prefs Opts.showHelpOnEmpty) cliProg

    if testIsomorphic then
        Environment.withArgs [] $ Tasty.defaultMain $ Test.testIsoOn scriptPath

    else do
        script <- Cereal.runGet Script.decodeScript <$> BS.readFile scriptPath
        case script of
            Right s -> do
                --Pretty.pPrint s
                Weigh.mainWith (Weigh.value "script" s)

            Left e -> putStrLn e

