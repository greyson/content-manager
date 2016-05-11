{-# LANGUAGE CPP #-}
module Data.Content.Types
       ( CMS, CMSError(..)
       , cms
       , cmsRoot

       , runCMS
       , MonadCMS

       , Tag
       , getTag

       , CMSFilePath
       , cmsFilePath
       , cmsAbsolute
       , cmsCanonical

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
import           Data.Content.Utils
import           Data.Char             (toLower)
import           Data.List             (isPrefixOf)
import qualified Data.Set              as Set
import qualified System.Directory      as Posix
import           System.FilePath
import           System.IO.Error
import qualified System.Posix.Files    as Posix
import           Text.Bytedump         (dumpRawBS)

data CMSError = CMSError String
              | CMSIOError IOError
              | OutsideCMS
              | NotInCMS
              | NoSuchTag String
              | AlreadyExists
              deriving Show

data CMS = CMS { cmsDir :: FilePath
               }
           deriving Show

type MonadCMS a = ReaderT CMS (ExceptT CMSError IO) a

-- |'cms' /path/ instantiates the CMS which is at, or contains /path/
cms :: FilePath -> IO CMS
cms fp = do
  hasAll <- Posix.doesDirectoryExist (fp </> _all)
  case hasAll of
   True -> return $ CMS fp
   False -> do
     let next = takeDirectory fp
     if isDrive next then error "No content directory found"
                     else cms next

-- | Run a CMS action with a given CMS.
runCMS :: MonadCMS a -> CMS -> IO (Either CMSError a)
runCMS a cms = runExceptT $ runReaderT a cms

-- | Get the CMS's root directory
cmsRoot :: MonadCMS FilePath
cmsRoot = cmsDir <$> ask

newtype Tag = Tag { unTag :: String }
            deriving Eq

instance Show Tag where
  show (Tag t) = t

-- | 'getTag' /tagname/ obtains a 'Tag' with the given /tagname/ as
-- long as it exists within the CMS.
getTag :: String -> MonadCMS Tag
getTag tagname = do
  top <- cmsRoot
  tagExists <- liftIO $ Posix.doesDirectoryExist (top </> _tags </> tagname </> _not)
  unless tagExists $ throwError (NoSuchTag tagname)
  return $ Tag tagname

-- | 'CMSFilePath' is a file path within the CMS, relative to its
-- root. This type is opaque so as to protect functions assuming they
-- have an internal path.
newtype CMSFilePath = CMSFilePath { unCmsPath :: FilePath }
                    deriving Show

-- | 'cmsFilePath' /path/ will attempt to obtain a CMS-relative
-- filepath for a given filepath of unknown origin (from the command
-- line, for instance).
cmsFilePath :: FilePath -> MonadCMS CMSFilePath
cmsFilePath fp = do
  top <- cmsRoot
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
thingTagPath tag thing = CMSFilePath $ _tags </> unTag tag </> unId (uniqueName thing)

thingNotagPath :: Tag -> Thing -> CMSFilePath
thingNotagPath tag thing = CMSFilePath $ _tags </> unTag tag </> _not </> unId (uniqueName thing)

cmsAbsolute :: CMSFilePath -> MonadCMS FilePath
cmsAbsolute (CMSFilePath cfp) = do
  top <- cmsRoot
  return $ top </> cfp

cmsCanonical :: CMSFilePath -> MonadCMS FilePath
cmsCanonical (CMSFilePath cfp) = do
  top <- cmsRoot
  cmsIO $ Posix.canonicalizePath (top </> cfp)

createLink :: CMSFilePath -> CMSFilePath -> MonadCMS ()
createLink linkTo anchor = do
  let to = cmsRelativise anchor linkTo
  at <- cmsAbsolute anchor
  cmsIO $ Posix.createSymbolicLink to at

-- | 'cmsIO' acts like 'liftIO' with exception handling.
cmsIO :: IO a -> MonadCMS a
cmsIO = lift . withExceptT toCMSError . ExceptT . tryIOError
  where
    toCMSError io
      | isAlreadyExistsError io = AlreadyExists
      | otherwise               = CMSIOError io

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

--
-- Retrieval
--

things :: MonadCMS (Set.Set Thing)
things = thingList (CMSFilePath _all)

tagThings :: Tag -> MonadCMS (Set.Set Thing)
tagThings t = thingList (CMSFilePath $ _tags </> unTag t)

notagThings :: Tag -> MonadCMS (Set.Set Thing)
notagThings t = thingList (CMSFilePath $ _tags </> unTag t </> _not)


thingList :: CMSFilePath -> MonadCMS (Set.Set Thing)
thingList dir = do
  abs <- cmsAbsolute dir
  tops <- liftIO $ filter isUniTop <$> Posix.getDirectoryContents abs
  uniqs <- liftIO $ mapM (\t -> fmap (t </>) <$> getUniSubs (abs </> t)) tops
  return $ Set.fromList $ map (\u -> Thing (Identifier u) Nothing) $ concat uniqs

isUniTop x = not (isHidden x) && (length x == 2)
isUniSub x = not (isHidden x) && (length (dropExtensions x) == 38)
getUniSubs dirInTop = filter isUniSub <$> Posix.getDirectoryContents dirInTop

--
-- Use outside
--

thingAbsolutePath :: Thing -> MonadCMS FilePath
thingAbsolutePath t = cmsAbsolute $ thingPath t

thingCanonicalPath :: Thing -> MonadCMS FilePath
thingCanonicalPath t = thingAbsolutePath t >>= cmsIO . Posix.canonicalizePath

--
-- General purpose utilities
--

shasum :: FilePath -> IO String
shasum fp = dumpRawBS . hashlazy <$> BL.readFile fp

uniqueNameFromFileContents :: FilePath -> IO Identifier
uniqueNameFromFileContents fp = do
  sha <- shasum fp
  let ext = fmap toLower (takeExtension fp)
  return $ Identifier $ (take 2 sha) </> (drop 2 sha) <.> ext

cmsRelativise :: CMSFilePath -> CMSFilePath -> FilePath
cmsRelativise (CMSFilePath from) (CMSFilePath target) = relativise from target
