module Data.Content.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.Hash.SHA1      (hashlazy)
import qualified Data.ByteString.Lazy   as BL
import           Data.Char             (toLower)
import           System.Directory      (makeAbsolute)
import           System.FilePath
import           Text.Bytedump         (dumpRawBS)

data CMSError = CMSError String
              | NotInCMS
              | NoSuchTag String
              | AlreadyExists
              deriving Show

data CMS = CMS { cmsDir :: FilePath
               }
           deriving Show

newtype CMSFilePath = CMSFilePath { unCmsPath :: FilePath }

type MonadCMS a = ReaderT CMS (ExceptT CMSError IO) a

newtype Identifier = Identifier { unId :: FilePath }
                   deriving (Eq, Ord)

instance Show Identifier where
  show = unId

data Thing = Thing { uniqueName :: Identifier
                   , path       :: Maybe FilePath
                   }

instance Eq Thing where
  left == right = (uniqueName left) == (uniqueName right)

instance Ord Thing where
  compare left right = compare (uniqueName left) (uniqueName right)

instance Show Thing where
  show thing = "\"" ++ (unId $ uniqueName thing) ++ "\"" ++
               case path thing of
                Nothing -> ""
                Just x -> "(\"" ++ x ++ "\")"

thingFromFile :: CMS -> FilePath -> IO Thing
thingFromFile = undefined

askCmsDir :: MonadCMS FilePath
askCmsDir = cmsDir <$> ask

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
  stripMetaDirs <$> makeAbsolute fp

-- | Relativise a path from the first file to the second.
--
-- When used with files having a common path (from, target), the
-- common path will be stripped off and the appropriate '..'
-- directories will be entered such that the returned path may be the
-- contents of a symbolic link to file `target` when placed at file
-- location `from`
relativise :: FilePath -> FilePath -> FilePath
relativise from target =
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
