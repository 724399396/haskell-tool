import System.Directory
import Control.Monad
import System.FilePath ((</>))

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
