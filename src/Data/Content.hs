{-# LANGUAGE CPP #-}
module Data.Content where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.Hash.SHA1     (hashlazy)
import           Data.Bool            (bool)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
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

type MonadCMS a = ReaderT CMS (ExceptT CMSError IO) a

runCMS :: MonadCMS a -> CMS -> IO (Either CMSError a)
runCMS a cms = runExceptT $ runReaderT a cms

allDir = (</> _all) . cmsDir
tagDir t = (</> t) . (</> _tags) . cmsDir

shasum :: FilePath -> IO String
shasum fp = dumpRawBS . hashlazy <$> BL.readFile fp

cmsThingFromInternalFile :: CMS -> String -> IO (Either CMSError Thing)
cmsThingFromInternalFile cms afp =
  let top = cmsDir cms

      fromUniqueName f = do
        theP <- canonicalizePath (top </> afp)
        return $ Right $ Thing (Identifier f) (Just theP)

      fromActualFile f = do
        theP <- canonicalizePath (top </> afp)
        ss <- shasum (top </> afp)
        let ext = fmap toLower (takeExtensions f)
            uni = (take 2 ss) </> (drop 2 ss) <.> ext
        if top `isPrefixOf` theP
          then return $ Right $ Thing (Identifier uni) (Just theP)
          else return $ Left NotInCMS

  in case splitDirectories afp of
      [_all, x, y] -> fromUniqueName (x </> y)
      [_tags, _, x, y] -> fromUniqueName (x </> y)
      other -> fromActualFile (joinPath other)

almostCanonicalizePath :: FilePath -> IO FilePath
almostCanonicalizePath fp = do
  stripMetaDirs <$> makeAbsolute fp

stripMetaDirs :: FilePath -> FilePath
stripMetaDirs fp = joinPath $ reverse $ dropMetas (reverse (splitDirectories fp), 0)
  where dropMetas ("..":rest, drp)  = dropMetas (rest, drp +1)
        dropMetas (".":rest, drp)   = dropMetas (rest,drp)
        dropMetas (x:rest, 0)       = x:(dropMetas (rest,0))
        dropMetas (_:rest, drp)     = dropMetas (rest, drp -1)
        dropMetas ([], 0)           = []
        dropMetas ([], _)           = error "Cannot drop more directories"

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
  lift $ ExceptT $ cmsThingFromInternalFile cms (makeRelative top afp)

cmsThingFromFile :: CMS -> FilePath -> IO (Either CMSError Thing)
cmsThingFromFile cms fp = do
  afp <- almostCanonicalizePath fp
  if (cmsDir cms) `isPrefixOf` afp
     then cmsThingFromInternalFile cms (makeRelative (cmsDir cms) afp)
     else return $ Left NotInCMS

askCmsDir :: MonadCMS FilePath
askCmsDir = cmsDir <$> ask

cmsImport :: FilePath -> MonadCMS ()
cmsImport file = do
  top <- askCmsDir
  cfp <- liftIO $ canonicalizePath file
  unless (top `isPrefixOf` cfp) (throwError NotInCMS)

  ss <- liftIO $ shasum (top </> file)
  let ext = fmap toLower (takeExtensions file)
      uni = (take 2 ss) </> (drop 2 ss) <.> ext
      linkAt = top </> _all </> uni
      linkTo = ".." </> ".." </> file

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
      linkTo = ".." </> ".." </> ".." </> _all </> sfp
      removable = top </> _tags </> tag </> _not </> sfp

  cmsMakeLink tag linkAt linkTo removable

cmsUntag :: String -> Thing -> MonadCMS ()
cmsUntag tag thing = do
  top <- askCmsDir
  let sfp = unId (uniqueName thing)
      linkAt = top </> _tags </> tag </> _not </> sfp
      linkTo = ".." </> ".." </> ".." </> ".." </> _all </> sfp
      removable = top </> _tags </> tag </> sfp

  cmsMakeLink tag linkAt linkTo removable

isHidden ('.':_) = True
isHidden _ = False

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
