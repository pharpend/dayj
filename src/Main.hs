module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Lazy as H
import Data.Time
import Data.Yaml
import System.Directory
import Text.Editor

main :: IO ()
main =
  do dirPath <- getXdgDirectory XdgData "dayj"
     createDirectoryIfMissing True dirPath
     let fp = mappend dirPath "/journal.yaml"
     fexists <- doesFileExist fp
     existingJournal <-
       if not fexists
          then return mempty
          else decodeFileEither fp >>=
               \case
                 Left perr -> fail $ show perr
                 Right foo -> return foo
     today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
     let todaysEntry =
           case H.lookup (formatDay today) existingJournal of
             Nothing -> mempty
             Just x -> x
     result <-
       runUserEditorDWIM plainTemplate
                         (C.pack todaysEntry)
     let newJournal =
           H.insert (formatDay today)
                    (C.unpack result)
                    existingJournal
     encodeFile fp newJournal

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale dayFormatString

dayFormatString :: String
dayFormatString = "%A, %B %-e, %Y (%Z)"
