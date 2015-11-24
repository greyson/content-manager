module Main where

import           Data.Content
import           Data.Content.Types
import qualified Data.Set           as Set
import           System.Directory
import           System.Environment (getArgs)
import           System.IO          (hPutStrLn, stderr)

runCMS' :: IO (Either CMSError a) -> IO a
runCMS' action = action >>= either (error . show) return

main = do
  cms <- getCurrentDirectory >>= cmsFrom
  command <- getArgs
  case command of
   ("all":filters) -> cmsListAll cms >>= doFilters cms filters
   ("tag":tag:files) -> mapM_ (\fp -> runCMS (cmsResolve fp >>= cmsTag tag) cms) files
   ("untag":tag:files) -> mapM_ (\fp -> runCMS (cmsResolve fp >>= cmsUntag tag) cms) files
   ("import":files) -> mapM_ (\f -> runCMS (cmsImport f) cms) files

doFilters :: CMS -> [String] -> Set.Set Thing -> IO ()
doFilters cms [] set =
  mapM_ (\x -> getPath cms x >>= putStrLn) (Set.elems set)
doFilters cms ["actual"] set =
  mapM_ (\x -> getActualPath cms x >>= putStrLn) (Set.elems set)

doFilters cms ("with":tag:remaining) set =
  Set.intersection set <$> cmsListTag cms tag >>= doFilters cms remaining
doFilters cms ("not":tag:remaining) set =
  Set.intersection set <$> cmsListUntag cms tag >>= doFilters cms remaining

doFilters cms ("lacking":tag:remaining) set = do
  allIn <- cmsListTag cms tag
  allOut <- cmsListUntag cms tag
  let all = Set.union allIn allOut
  doFilters cms remaining $ Set.filter (flip Set.notMember all) set

doFilters cms (other:_) set = do
  hPutStrLn stderr $ "Unknown filter '" ++ other ++ "'"
