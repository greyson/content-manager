{-# LANGUAGE CPP #-}
module Data.Content where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char            (toLower)
import           Data.Content.Types
import           Data.List            (sort, isPrefixOf)
import qualified Data.Set             as Set
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           Text.Bytedump        (dumpRawBS)

#define _all  "ALL"
#define _tags "TAGS"
#define _not  ".not"

runCMS = runCMSWithCMS

runCMSWithCMS :: MonadCMS a -> CMS -> IO (Either CMSError a)
runCMSWithCMS a cms = runExceptT $ runReaderT a cms

runCMSLocally :: MonadCMS a -> IO (Either CMSError a)
runCMSLocally act =
  getCurrentDirectory >>= cmsFrom >>= runCMSWithCMS act

cmsFrom :: FilePath -> IO CMS
cmsFrom fp = do
  hasAll <- doesDirectoryExist (fp </> _all)
  case hasAll of
   True -> return $ CMS fp
   False -> do
     let next = takeDirectory fp
     if isDrive next then error "No content directory found"
                     else cmsFrom next

cmsResolve :: FilePath -> MonadCMS Thing
cmsResolve fp = do
  top <- askCmsDir
  afp <- liftIO $ almostCanonicalizePath fp
  unless (top `isPrefixOf` afp) $ throwError NotInCMS
  cms <- ask
  cmsThingFromInternalFile cms (makeRelative top afp)

cmsImport :: FilePath -> MonadCMS ()
cmsImport file = do
  top <- askCmsDir
  cfp <- liftIO $ canonicalizePath file
  unless (top `isPrefixOf` cfp) (throwError NotInCMS)

  uni <- liftIO $ uniqueNameFromFileContents cfp
  let linkAt = top </> _all </> unId uni
      linkTo = relativise linkAt cfp

  liftIO $ doesFileExist linkAt >>= flip when (removeFile linkAt)
  liftIO $ createDirectoryIfMissing True (takeDirectory linkAt)
  liftIO $ createSymbolicLink linkTo linkAt

cmsMakeLink :: String -> FilePath -> FilePath -> FilePath -> MonadCMS ()
cmsMakeLink tag linkAt linkTo removable = do
  top <- askCmsDir

  liftIO (doesDirectoryExist (top </> _tags </> tag)) >>=
    flip unless (throwError $ NoSuchTag tag)
  liftIO $ doesFileExist removable >>= flip when (removeFile removable)
  liftIO $ createDirectoryIfMissing True (takeDirectory linkAt)
  liftIO $ createSymbolicLink linkTo linkAt

cmsTag :: String -> Thing -> MonadCMS ()
cmsTag tag thing = do
  top <- askCmsDir
  let sfp = unId (uniqueName thing)
      linkAt = top </> _tags </> tag </> sfp
      linkTo = relativise linkAt (top </> _all </> sfp)
      removable = top </> _tags </> tag </> _not </> sfp

  cmsMakeLink tag linkAt linkTo removable

cmsUntag :: String -> Thing -> MonadCMS ()
cmsUntag tag thing = do
  top <- askCmsDir
  let sfp = unId (uniqueName thing)
      linkAt = top </> _tags </> tag </> _not </> sfp
      linkTo = relativise linkAt (top </> _all </> _not </> sfp)
      removable = top </> _tags </> tag </> sfp

  cmsMakeLink tag linkAt linkTo removable

listThings :: CMS -> FilePath -> IO (Set.Set Thing)
listThings cms dir = do
  let isTop x = not (isHidden x) && (length x == 2)
      isSub x = not (isHidden x) && (length (dropExtension x) == 38)
      getSubs dirInTop = filter isSub <$> getDirectoryContents dirInTop
      d = cmsDir cms </> dir

  tops <- filter isTop <$> getDirectoryContents d
  uniqs <- mapM (\t -> fmap (t </>) <$> getSubs (d </> t)) tops
  return $ Set.fromList $ map (\u -> Thing (Identifier u) Nothing) $ concat uniqs

cmsListAll :: CMS -> IO (Set.Set Thing)
cmsListAll cms = listThings cms _all

cmsListTag :: CMS -> String -> IO (Set.Set Thing)
cmsListTag cms tag = do
  listThings cms (_tags </> tag)

cmsListUntag :: CMS -> String -> IO (Set.Set Thing)
cmsListUntag cms tag = do
  listThings cms (_tags </> tag </> _not)

getPath :: CMS -> Thing -> IO FilePath
getPath cms thing =
  makeRelativeToCurrentDirectory $ cmsDir cms </> _all </> unId (uniqueName thing)

getActualPath :: CMS -> Thing -> IO FilePath
getActualPath cms thing =
  case path thing of
   Just p -> return p
   Nothing -> canonicalizePath (cmsDir cms </> _all </> unId (uniqueName thing))

cmsThingFromInternalFile :: CMS -> String -> MonadCMS Thing
cmsThingFromInternalFile cms afp = do
  top <- askCmsDir
  thePath <- liftIO $ canonicalizePath (top </> afp)
  unless (top `isPrefixOf` thePath) $ throwError NotInCMS

  let fromUniqueName id =
        return $ Thing (Identifier id) (Just thePath)

      fromActualFile file = do
        id <- liftIO $ uniqueNameFromFileContents thePath
        return $ Thing id (Just thePath)

  case splitDirectories afp of
    [_all, x, y] -> fromUniqueName (x </> y)
    [_tags, _, x, y] -> fromUniqueName (x </> y)
    [_tags, _, _not, x, y] -> fromUniqueName (x </> y)
    other -> fromActualFile afp
