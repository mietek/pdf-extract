{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative ((<$>), (<*>), (<>), Parser, ParserInfo)
import System.IO (IOMode (..), withBinaryFile)

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.IO as T
import qualified Options.Applicative as O
import qualified Pdf.Toolbox.Content.Processor as P
import qualified Pdf.Toolbox.Content.Transform as P
import qualified Pdf.Toolbox.Document.Pdf as P
import qualified Pdf.Toolbox.Document.Document as P
import qualified Pdf.Toolbox.Document.Catalog as P
import qualified Pdf.Toolbox.Document.PageNode as P
import qualified Pdf.Toolbox.Document.Page as P


data Glyph = Glyph
    { _left   :: Double
    , _top    :: Double
    , _right  :: Double
    , _bottom :: Double
    , _text   :: Text
    }
  deriving (Generic, Show)


instance ToJSON Glyph where
  toJSON =
      J.genericToJSON J.defaultOptions
        { J.fieldLabelModifier = drop 1 }


encodePages :: [[[Glyph]]] -> ByteString
encodePages inputs =
    J.encodePretty' config inputs
  where
    config = J.Config 4 $ J.keyOrder
      [ "left"
      , "top"
      , "right"
      , "bottom"
      , "text"
      ]


convertGlyphs :: [[P.Glyph]] -> [[Glyph]]
convertGlyphs inputs =
    filter (not . null) (map (mapMaybe convertGlyph) inputs)
  where
    convertGlyph input =
        case P.glyphText input of
          Nothing   -> Nothing
          Just text ->
            Just $ Glyph
              { _left   = left
              , _top    = top
              , _right  = right
              , _bottom = bottom
              , _text   = text
              }
      where
        P.Vector left top     = P.glyphTopLeft input
        P.Vector bottom right = P.glyphBottomRight input


data Options = Options
    { optPassword :: Maybe String
    , optJSON     :: Bool
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
      O.switch (
        O.long "json" <>
        O.short 'j' <>
        O.help "Output in JSON format" ) <*>
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
        if optJSON opts
          then do
            pages <- forM [0 .. pageCount - 1] $ \pageNumber -> do
              page <- P.pageNodePageByNum rootNode pageNumber
              P.pageExtractGeneric convertGlyphs page
            liftIO (L.putStrLn (encodePages pages))
          else do
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
