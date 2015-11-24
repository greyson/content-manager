module Data.Content.Types where

data CMSError = CMSError String
              | NotInCMS
              | NoSuchTag String
              | AlreadyExists
              deriving Show

data CMS = CMS { cmsDir :: FilePath
               }
           deriving Show

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
