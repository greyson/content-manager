{-# LANGUAGE CPP #-}
module Data.Content.Types
       ( CMS, cmsFrom
       , runCMS

       , CMSError(..)
       , Tag
       , CMSFilePath, cmsFilePath
       , cmsAbsolute, cmsCanonical

       , MonadCMS
       , Thing, thingPath, thingTagPath, thingNotagPath, thingFromFile

       , things, tagThings, notagThings

       , thingAbsolutePath, thingCanonicalPath

       , generateThingPath
       , createLink
       ) where

#define _all  "ALL"
#define _tags "TAGS"
#define _not  ".not"

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.Hash.SHA1      (hashlazy)
import qualified Data.ByteString.Lazy   as BL
import           Data.Char             (toLower)
import           Data.List             (isPrefixOf)
import qualified Data.Set              as Set
import qualified System.Directory      as Posix
import           System.FilePath
import qualified System.Posix.Files    as Posix
import           Text.Bytedump         (dumpRawBS)

data CMSError = CMSError String
              | OutsideCMS
              | NotInCMS
              | NoSuchTag String
              | AlreadyExists
              deriving Show

data CMS = CMS { cmsDir :: FilePath
               }
           deriving Show

cmsFrom :: FilePath -> IO CMS
cmsFrom fp = do
  hasAll <- Posix.doesDirectoryExist (fp </> _all)
  case hasAll of
   True -> return $ CMS fp
   False -> do
     let next = takeDirectory fp
     if isDrive next then error "No content directory found"
                     else cmsFrom next

runCMS :: MonadCMS a -> CMS -> IO (Either CMSError a)
runCMS a cms = runExceptT $ runReaderT a cms

type Tag = String

newtype CMSFilePath = CMSFilePath { unCmsPath :: FilePath }
                    deriving Show

cmsFilePath :: FilePath -> MonadCMS CMSFilePath
cmsFilePath fp = do
  top <- askCmsDir
  afp <- liftIO $ almostCanonicalizePath fp
  unless (top `isPrefixOf` afp) $ throwError OutsideCMS
  return $ CMSFilePath $ makeRelative top afp

generateThingPath :: FilePath -> MonadCMS CMSFilePath
generateThingPath fp = do
  uni <- liftIO $ uniqueNameFromFileContents fp
  return $ CMSFilePath $ _all </> unId uni

thingPath :: Thing -> CMSFilePath
thingPath thing = CMSFilePath $ _all </> unId (uniqueName thing)

thingTagPath :: Tag -> Thing -> CMSFilePath
thingTagPath tag thing = CMSFilePath $ _tags </> tag </> unId (uniqueName thing)

thingNotagPath :: Tag -> Thing -> CMSFilePath
thingNotagPath tag thing = CMSFilePath $ _tags </> tag </> _not </> unId (uniqueName thing)

cmsAbsolute :: CMSFilePath -> MonadCMS FilePath
cmsAbsolute (CMSFilePath cfp) = do
  top <- askCmsDir
  return $ top </> cfp

createLink :: CMSFilePath -> CMSFilePath -> MonadCMS ()
createLink linkTo anchor =
  let to = relativise anchor linkTo
  in do
     at <- cmsAbsolute anchor
     liftIO $ Posix.createSymbolicLink to at

cmsCanonical :: CMSFilePath -> MonadCMS FilePath
cmsCanonical (CMSFilePath cfp) = do
  top <- askCmsDir
  liftIO $ Posix.canonicalizePath (top </> cfp)

type MonadCMS a = ReaderT CMS (ExceptT CMSError IO) a

newtype Identifier = Identifier { unId :: FilePath }
                   deriving (Eq, Ord)

instance Show Identifier where
  show = unId

data Thing = Thing { uniqueName :: Identifier
                   , path       :: Maybe CMSFilePath
                   }

instance Eq Thing where
  left == right = (uniqueName left) == (uniqueName right)

instance Ord Thing where
  compare left right = compare (uniqueName left) (uniqueName right)

instance Show Thing where
  show thing = "\"" ++ (unId $ uniqueName thing) ++ "\"" ++
               case path thing of
                Nothing -> ""
                Just x -> "(\"" ++ show x ++ "\")"

thingFromFile :: FilePath -> MonadCMS Thing
thingFromFile raw = do
  cmsPath <- cmsFilePath raw

  let fromUniqueName id = return $ Thing (Identifier id) (Just cmsPath)

  case splitDirectories (unCmsPath cmsPath) of
    [_all, x, y]           -> fromUniqueName (x </> y)
    [_tags, _, x, y]       -> fromUniqueName (x </> y)
    [_tags, _, _not, x, y] -> fromUniqueName (x </> y)
    other                  -> do
      id <- liftIO $ uniqueNameFromFileContents raw
      return $ Thing id (Just cmsPath)

askCmsDir :: MonadCMS FilePath
askCmsDir = cmsDir <$> ask

--
-- Retrieval
--

things :: MonadCMS (Set.Set Thing)
things = thingList (CMSFilePath _all)

tagThings :: Tag -> MonadCMS (Set.Set Thing)
tagThings t = thingList (CMSFilePath $ _tags </> t)

notagThings :: Tag -> MonadCMS (Set.Set Thing)
notagThings t = thingList (CMSFilePath $ _tags </> t </> _not)


thingList :: CMSFilePath -> MonadCMS (Set.Set Thing)
thingList dir = do
  abs <- cmsAbsolute dir
  tops <- liftIO $ filter isUniTop <$> Posix.getDirectoryContents abs
  uniqs <- liftIO $ mapM (\t -> fmap (t </>) <$> getUniSubs (abs </> t)) tops
  return $ Set.fromList $ map (\u -> Thing (Identifier u) Nothing) $ concat uniqs

isUniTop x = not (isHidden x) && (length x == 2)
isUniSub x = not (isHidden x) && (length (dropExtension x) == 38)
getUniSubs dirInTop = filter isUniSub <$> Posix.getDirectoryContents dirInTop

--
-- Use outside
--

thingAbsolutePath :: Thing -> MonadCMS FilePath
thingAbsolutePath t = cmsAbsolute $ thingPath t

thingCanonicalPath :: Thing -> MonadCMS FilePath
thingCanonicalPath t = thingAbsolutePath t >>= liftIO . Posix.canonicalizePath

--
-- General purpose utilities
--

shasum :: FilePath -> IO String
shasum fp = dumpRawBS . hashlazy <$> BL.readFile fp

uniqueNameFromFileContents :: FilePath -> IO Identifier
uniqueNameFromFileContents fp = do
  sha <- shasum fp
  let ext = fmap toLower (takeExtensions fp)
  return $ Identifier $ (take 2 sha) </> (drop 2 sha) <.> ext

stripMetaDirs :: FilePath -> FilePath
stripMetaDirs fp = joinPath $ reverse $ dropMetas (reverse (splitDirectories fp), 0)
  where dropMetas ("..":rest, drp)  = dropMetas (rest, drp +1)
        dropMetas (".":rest, drp)   = dropMetas (rest,drp)
        dropMetas (x:rest, 0)       = x:(dropMetas (rest,0))
        dropMetas (_:rest, drp)     = dropMetas (rest, drp -1)
        dropMetas ([], 0)           = []
        dropMetas ([], _)           = error "Cannot drop more directories"

isHidden ('.':_) = True
isHidden _ = False

-- | A best-effort path canonicalization without symlink dereferencing.
almostCanonicalizePath :: FilePath -> IO FilePath
almostCanonicalizePath fp = do
  stripMetaDirs <$> Posix.makeAbsolute fp

-- | Relativise a path from the first file to the second.
--
-- When used with files having a common path (from, target), the
-- common path will be stripped off and the appropriate '..'
-- directories will be entered such that the returned path may be the
-- contents of a symbolic link to file `target` when placed at file
-- location `from`
relativise' :: FilePath -> FilePath -> FilePath
relativise' from target =
  let fromS = splitDirectories from
      targS = splitDirectories target
      removeBase (x:restx) (y:resty)
        | x == y    = removeBase restx resty
        | otherwise = (x:restx, y:resty)
      ascension (x:[])   = "."
      ascension (x:[_])  = ".."
      ascension (x:rest) = ".." </> ascension rest

      (fromR, targR) = removeBase fromS targS
  in ascension fromR </> joinPath targR

relativise :: CMSFilePath -> CMSFilePath -> FilePath
relativise (CMSFilePath from) (CMSFilePath target) = relativise' from target
