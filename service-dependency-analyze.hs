{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import Control.Monad
import Data.Maybe
import System.FilePath ((</>), takeExtension)
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

typeVarName :: FullClass -> FilePath -> IO (Maybe (FilePath, Class))
typeVarName (ns, n) fp = do con <- TIO.readFile fp
                            if (isInfixOf ns con && isInfixOf (append n " ") con)
                              then (let
                                      varName :: Maybe Text
                                      varName = listToMaybe $ Prelude.dropWhile (/= "n") $ T.words con
                                   in
                                     return (fmap ((,) fp) varName))
                              else return $ Nothing


httpSendFind :: FilePath -> IO [FilePath]
httpSendFind fp = do fs <- getFiles fp
                     let
                       csFs :: [FilePath]
                       csFs = filter ((==) ".cs" . pack . takeExtension) fs
                     fmap maybeToList <$> traverse (typeVarName ("Harness.Rest.Client", "IRestClient")) csFs

main :: IO ()
main = putStrLn ""
