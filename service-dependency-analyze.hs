{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.IO
import Control.Monad
import Data.Maybe
import System.FilePath ((</>), takeExtension, splitDirectories)
import Data.Text as T hiding (map, filter)
import qualified Data.Text.IO as TIO

type FileFilter = FilePath -> IO Bool

notSpecialEntrie :: FilePath -> Bool
notSpecialEntrie = flip notElem [".", ".."]

getFiles :: FilePath -> IO [FilePath]
getFiles fp = do isDirectory <- doesDirectoryExist fp
                 if (isDirectory)
                   then (do fs <- getDirectoryContents fp
                            fss <- traverse getFiles $ map (fp </>) $ filter notSpecialEntrie fs
                            return (join fss))
                   else return [fp]

type Namespace = Text
type Class = Text
type FullClass = (Namespace, Class)

readAsISO88591 :: FilePath -> IO Text
readAsISO88591 fp = do h <- openFile fp ReadMode
                       hSetEncoding h latin1
                       TIO.hGetContents h

typeVarName :: FullClass -> FilePath -> IO (Maybe (FilePath, Class))
typeVarName (ns, n) fp = do con <- readAsISO88591 fp
                            if (isInfixOf ns con && isInfixOf n con)
                              then (let
                                      varName :: Maybe Text
                                      varName = listToMaybe $ Prelude.dropWhile (/= n) $ T.words con
                                   in
                                     return (fmap ((,) fp) varName))
                              else return $ Nothing

filePathToModuleAndFile :: FilePath -> (String, String, String)
filePathToModuleAndFile fp = let ps = splitDirectories fp
                             in
                               (Prelude.head $ Prelude.drop 2 $ Prelude.dropWhile (/= "") ps, Prelude.head $ Prelude.drop 1 $ Prelude.reverse ps, Prelude.last ps)

httpSendFind :: FilePath -> IO [(String,String, String)]
httpSendFind fp = do fs <- getFiles fp
                     let
                       csFs :: [FilePath]
                       csFs = filter ((==) ".cs" . pack . takeExtension) fs
                     fmap (filePathToModuleAndFile . fst) . join . fmap maybeToList <$> traverse (typeVarName ("Harness.Rest.Client", "IRestClient")) csFs

main :: IO ()
main = do fs <- httpSendFind ""
          mapM_ print fs
