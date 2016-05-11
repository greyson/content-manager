module Data.Content.Utils where

import Control.Monad (when, unless)
import System.Directory
import System.FilePath

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

-- | A best-effort path canonicalization without symlink dereferencing.
almostCanonicalizePath :: FilePath -> IO FilePath
almostCanonicalizePath fp = do
  stripMetaDirs <$> makeAbsolute fp

isHidden ('.':_) = True
isHidden _ = False

stripMetaDirs :: FilePath -> FilePath
stripMetaDirs fp = joinPath $ reverse $ dropMetas (reverse (splitDirectories fp), 0)
  where dropMetas ("..":rest, drp)  = dropMetas (rest, drp +1)
        dropMetas (".":rest, drp)   = dropMetas (rest,drp)
        dropMetas (x:rest, 0)       = x:(dropMetas (rest,0))
        dropMetas (_:rest, drp)     = dropMetas (rest, drp -1)
        dropMetas ([], 0)           = []
        dropMetas ([], _)           = error "Cannot drop more directories"

whenM :: Monad m => m Bool -> m () -> m ()
whenM test act = test >>= flip when act

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test act = test >>= flip unless act

-- | 'allow' is "flip when" for use when the boolean argument to
-- 'when' comes from a monadic function.
allow :: Monad m => m () -> Bool -> m ()
allow = flip when
