{-# LANGUAGE CPP #-}
module Data.Content
       ( CMSError(..)
       , CMS, cmsFrom, cmsRoot

       , MonadCMS, runCMS

       , Thing
       , thingFromFile, thingAbsolutePath, thingCanonicalPath
       , things, tagThings, notagThings

       , Tag
       , getTag

       , cmsImport, cmsTag, cmsNotag
       , cleanRootLinks
       ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char            (toLower)
import           Data.Content.Types
import           Data.List            (sort, isPrefixOf)
import           Data.Maybe           (isNothing)
import qualified Data.Set             as Set
import           System.Directory
import           System.FilePath
import           Text.Bytedump        (dumpRawBS)

cmsImport :: FilePath -> MonadCMS ()
cmsImport file = do
  -- Canonicalize the original path
  truefile <- liftIO $ canonicalizePath file

  -- Find a CMS-relative for it.
  crfp     <- cmsFilePath truefile

  -- Create the 'all' path at which it will be indexed.
  linkAt <- generateThingPath truefile

  at <- cmsAbsolute linkAt

  liftIO $ doesFileExist at >>= flip when (removeFile at)
  liftIO $ createDirectoryIfMissing True (takeDirectory at)
  createLink crfp linkAt
  liftIO $ putStrLn $ "Imported " ++ file

cmsTag :: Tag -> Thing -> MonadCMS ()
cmsTag tag thing = do
  let linkAt = thingTagPath tag thing
      unlink = thingNotagPath tag thing
      linkTo = thingPath thing

  rm <- cmsAbsolute unlink
  at <- cmsAbsolute linkAt
  liftIO $ doesFileExist rm >>= flip when (removeFile rm)
  liftIO $ createDirectoryIfMissing True (takeDirectory at)
  createLink linkTo linkAt

cmsNotag :: Tag -> Thing -> MonadCMS ()
cmsNotag tag thing = do
  let linkAt = thingNotagPath tag thing
      unlink = thingTagPath tag thing
      linkTo = thingPath thing

  rm <- cmsAbsolute unlink
  at <- cmsAbsolute linkAt
  liftIO $ doesFileExist rm >>= flip when (removeFile rm)
  liftIO $ createDirectoryIfMissing True (takeDirectory at)
  createLink linkTo linkAt

cleanRootLinks :: MonadCMS [FilePath]
cleanRootLinks = brokenRootLinks >>= mapM delete
  where delete l = thingAbsolutePath l >>= \f -> liftIO $ do
          removeFile f
          return f

catchMaybe a = (Just <$> a) `catchError` (return . const Nothing)

brokenRootLinks :: MonadCMS [Thing]
brokenRootLinks =
  map fst . filter (isNothing . snd) <$> rootLinks
  where rootLinks = do
          allthethings <- Set.elems <$> things
          zip allthethings <$> mapM (catchMaybe . thingCanonicalPath) allthethings
