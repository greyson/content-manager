{-# LANGUAGE CPP #-}
module Data.Content where

import           Crypto.Hash.SHA1     (hashlazy)
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

allDir = (</> _all) . cmsDir
tagDir t = (</> t) . (</> _tags) . cmsDir

shasum :: FilePath -> IO String
shasum fp = dumpRawBS . hashlazy <$> BL.readFile fp

cmsThingFromFile :: CMS -> FilePath -> IO (Either CMSError Thing)
cmsThingFromFile cms fp = do
  afp <- almostCanonicalizePath fp
  if (cmsDir cms) `isPrefixOf` afp
     then cmsThingFromInternalFile cms (makeRelative (cmsDir cms) afp)
     else return $ Left NotInCMS

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

cmsResolve :: CMS -> FilePath -> IO (Either CMSError Thing)
cmsResolve cms fp = cmsThingFromFile cms fp

cmsImport :: CMS -> FilePath -> IO (Either CMSError ())
cmsImport cms file = do
  cfp <- canonicalizePath file
  if (cmsDir cms) `isPrefixOf` cfp
     then cmsCreateThing cms (makeRelative (cmsDir cms) cfp)
     else return $ Left NotInCMS

cmsCreateThing :: CMS -> FilePath -> IO (Either CMSError ())
cmsCreateThing cms file = do
  ss <- shasum (cmsDir cms </> file)
  let ext = fmap toLower (takeExtensions file)
      uni = (take 2 ss) </> (drop 2 ss) <.> ext
      linkAt = cmsDir cms </> _all </> uni
      linkTo = ".." </> ".." </> file

  doesFileExist linkAt >>= \x -> case x of
    True -> removeFile linkAt
    False -> return ()
  createDirectoryIfMissing True (takeDirectory linkAt)
  putStrLn $ "Creating link to " ++ linkTo ++ " at " ++ linkAt
  Right <$> createSymbolicLink linkTo linkAt


cmsTag :: CMS -> String -> Thing -> IO (Either CMSError ())
cmsTag cms tag thing = do
  let sfp = unId (uniqueName thing)
      linkAt = cmsDir cms </> _tags </> tag </> sfp
      linkTo = ".." </> ".." </> ".." </> _all </> sfp
      removable = cmsDir cms </> _tags </> tag </> _not </> sfp

  doesDirectoryExist (cmsDir cms </> _tags </> tag) >>= \t -> case t of
    True -> do
      doesFileExist removable >>= \x -> case x of
        True -> removeFile removable
        False -> return ()
      createDirectoryIfMissing True (takeDirectory linkAt)
      Right <$> createSymbolicLink linkTo linkAt
    False -> return $ Left (NoSuchTag tag)

cmsUntag :: CMS -> String -> Thing -> IO (Either CMSError ())
cmsUntag cms tag thing = do
  let sfp = unId (uniqueName thing)
      linkAt = cmsDir cms </> _tags </> tag </> _not </> sfp
      linkTo = ".." </> ".." </> ".." </> ".." </> _all </> sfp
      removable = cmsDir cms </> _tags </> tag </> sfp

  doesDirectoryExist (cmsDir cms </> _tags </> tag) >>= \t -> case t of
    True -> do
      doesFileExist removable >>= \x -> case x of
        True -> removeFile removable
        False -> return ()
      createDirectoryIfMissing True (takeDirectory linkAt)
      Right <$> createSymbolicLink linkTo linkAt
    False -> return $ Left (NoSuchTag tag)

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
