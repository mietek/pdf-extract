module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import System.Environment (getArgs)
import System.IO (IOMode (..), withBinaryFile)

import qualified Data.Text.IO as T
import qualified Pdf.Toolbox.Document.Pdf as P
import qualified Pdf.Toolbox.Document.Document as P
import qualified Pdf.Toolbox.Document.Catalog as P
import qualified Pdf.Toolbox.Document.PageNode as P
import qualified Pdf.Toolbox.Document.Page as P


extract :: FilePath -> IO ()
extract file =
    withBinaryFile file ReadMode $ \handle -> do
      result <- P.runPdfWithHandle handle P.knownFilters $ do
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
    args <- getArgs
    case args of
      [] -> error "Usage: pdf-extract FILE [...]"
      files -> mapM_ extract files
