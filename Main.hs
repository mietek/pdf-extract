module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, when)
import Options.Applicative ((<$>), (<*>), (<>), Parser, ParserInfo)
import System.IO (IOMode (..), withBinaryFile)

import qualified Data.ByteString.Char8 as S
import qualified Data.Text.IO as T
import qualified Options.Applicative as O
import qualified Pdf.Toolbox.Document.Pdf as P
import qualified Pdf.Toolbox.Document.Document as P
import qualified Pdf.Toolbox.Document.Catalog as P
import qualified Pdf.Toolbox.Document.PageNode as P
import qualified Pdf.Toolbox.Document.Page as P


data Options = Options
    { optPassword :: Maybe String
    , optFiles    :: [FilePath]
    }
  deriving (Show)


parseOptions :: Parser Options
parseOptions =
    Options <$>
      O.optional (O.strOption (
        O.metavar "PASSWORD" <>
        O.long "password" <>
        O.short 'p' <>
        O.help "Decryption password" )) <*>
      O.some (O.strArgument (
        O.metavar "FILE"))


withInfo :: Parser a -> String -> ParserInfo a
withInfo parser info =
    O.info (O.helper <*> parser) (O.progDesc info)


extract :: Options -> FilePath -> IO ()
extract opts file =
    withBinaryFile file ReadMode $ \handle -> do
      result <- P.runPdfWithHandle handle P.knownFilters $ do
        needPass <- P.isEncrypted
        when needPass $ do
          let pass = case optPassword opts of
                       Just str -> S.pack str
                       Nothing -> P.defaultUserPassword
          rightPass <- P.setUserPassword pass
          when (not rightPass) $
            error "Error: Wrong password"
        document <- P.document
        catalog <- P.documentCatalog document
        rootNode <- P.catalogPageNode catalog
        pageCount <- P.pageNodeNKids rootNode
        forM_ [0 .. pageCount - 1] $ \pageNumber -> do
          page <- P.pageNodePageByNum rootNode pageNumber
          text <- P.pageExtractText page
          liftIO (T.putStrLn text)
      case result of
        Left err -> error ("Error: " ++ show err)
        Right _ -> return ()


main :: IO ()
main = do
    opts <- O.execParser (parseOptions `withInfo` "Extract contents of PDF files")
    mapM_ (extract opts) (optFiles opts)
