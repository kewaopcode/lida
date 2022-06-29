module Main where

import qualified Data.ByteString     as BS
import qualified Data.Serialize      as Cereal
import           Options.Applicative
import qualified System.Environment  as Environment
import qualified Test.Tasty          as Tasty
import qualified Text.Pretty.Simple  as Pretty

import qualified LuaJIT.Script       as Script
import           Test                (testIsoOn)

data Options = Options
    { scriptPath     :: FilePath
    , testIsomorphic :: Bool
    , prettyPrint    :: Bool
    } deriving Show


options :: Parser Options
options = Options <$> filePath <*> iso <*> pretty where
    filePath =
        strArgument
             $ metavar "FILE"
            <> help "File you want to disassembly"
    iso =
        flag False True
             $ long "test-iso"
            <> help "Test if disassembler is isomorphic to assembler"

    pretty =
        flag False True
             $ short 'p'
            <> long "pretty"
            <> help "Print script structure pretty"


cliProg :: ParserInfo Options
cliProg =
    info (options <**> helper) (fullDesc <> header "LIDA - LuaJIT Interactive Disassembler")


main :: IO ()
main = do
    Options{..} <- customExecParser (prefs showHelpOnEmpty) cliProg
    if testIsomorphic then
        Environment.withArgs [] $ Tasty.defaultMain $ testIsoOn scriptPath

    else do
        script <- Cereal.runGet Script.decodeScript <$> BS.readFile scriptPath
        case script of
            Right s ->
                if prettyPrint then
                    Pretty.pPrint s
                else
                    print s

            Left e -> putStrLn e

