module Main where

import           Control.Monad.Reader
import           Data.Content
import qualified Data.Set           as Set
import           System.Directory
import           System.Environment (getArgs)
import           System.IO          (hPutStrLn, stderr)

runCMS' :: IO (Either CMSError a) -> IO a
runCMS' action = action >>= either (error . show) return

failCMS :: Either CMSError a -> IO a
failCMS (Right a) = return a
failCMS (Left e) = error (show e)

runCommand :: CMS -> MonadCMS a -> IO a
runCommand cms action = runCMS action cms >>= failCMS

main = do
  cms <- getCurrentDirectory >>= cmsFrom
  command <- getArgs
  runCommand cms $ case command of
    ("import":files)    -> mapM_ cmsImport files
    ("all":filters)     -> things >>= doFilters filters

    ("tag":tagname:files)   -> mapM_ (doTag tagname) files
    ("untag":tagname:files) -> mapM_ (doNotag tagname) files

doTag tagname file = do
  thingFromFile file >>= cmsTag tagname
  liftIO $ hPutStrLn stderr $ tagname ++ " now includes '" ++ file ++ "'"

doNotag tagname file = do
  thingFromFile file >>= cmsNotag tagname
  liftIO $ hPutStrLn stderr $ tagname ++ " now excludes '" ++ file ++ "'"

doFilters :: [String] -> Set.Set Thing -> MonadCMS ()
doFilters [] set =
  mapM_ (\t -> thingAbsolutePath t >>= liftIO . putStrLn) (Set.elems set)
doFilters ["actual"] set =
  mapM_ (\t -> thingCanonicalPath t >>= liftIO . putStrLn) (Set.elems set)

doFilters ("with":tag:remaining) set =
  Set.intersection set <$> tagThings tag >>= doFilters remaining
doFilters ("not":tag:remaining) set =
  Set.intersection set <$> notagThings tag >>= doFilters remaining

doFilters ("lacking":tag:remaining) set = do
  allIn <- tagThings tag
  allOut <- notagThings tag
  let all = Set.union allIn allOut
  doFilters remaining $ Set.filter (flip Set.notMember all) set

doFilters (other:_) set = do
  liftIO $ hPutStrLn stderr $ "Unknown filter '" ++ other ++ "'"
