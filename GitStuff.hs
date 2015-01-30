{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module GitStuff where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Tagged
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getZonedTime)
import System.Process

import Git
import Git.Working (checkoutFiles)
import Git.Libgit2 as LG

class Blobbish a where
    makeBlobOid :: MonadGitLg m => a -> m LG.BlobOid

instance Blobbish T.Text where
    makeBlobOid = createBlobUtf8

instance Blobbish BS.ByteString where
    makeBlobOid = createBlob . BlobString

data HoldJson a = ToJSON a => HoldJson { fromHoldJson :: a }

instance ToJSON a => Blobbish (HoldJson a) where
    makeBlobOid = createBlob . BlobStringLazy . encode . fromHoldJson

class Pathish a where
    makeBS :: a -> BS.ByteString

instance Pathish T.Text where
    makeBS = encodeUtf8

instance Pathish BS.ByteString where
    makeBS = id

type MonadGitLg m = (MonadGit LgRepo m, MonadLg m)
type MonadGitLgExt m = (MonadGit LgRepo m, MonadLg m, MonadResource m)

type MonadsSuck = ReaderT LgRepo (NoLoggingT IO)
type MonadsSuckMore = ResourceT MonadsSuck

doIt :: MonadsSuckMore a -> MonadsSuck a
doIt = runResourceT

withRepo :: MonadsSuck a -> IO a
withRepo = withRepository' lgFactory opts

opts :: RepositoryOptions
opts = RepositoryOptions
    { repoPath = "repo"
    , repoWorkingDir = Just "repo"
    , repoIsBare = False
    , repoAutoCreate = True
    }

-- this stupid library has a default signature of "modified julian day 0",
-- or 20 years ago, and doesn't expose git's default signature
-- functionality (the thing that does what you *always* want)...
-- https://libgit2.github.com/libgit2/#HEAD/group/signature/git_signature_default
--
-- ... so we have to shell out to "git config" every single time
--
-- TODO(joel) - contribute back to gitlib
getSig :: IO Signature
getSig = do
    userName  <- readProcess "git" ["config", "user.name"] ""
    userEmail <- readProcess "git" ["config", "user.email"] ""
    time <- getZonedTime
    return $ Signature (fromString $ init userName) (fromString $ init userEmail) time

hasInitialCommit :: MonadGitLg m => m Bool
hasInitialCommit = isJust <$> Git.resolveReference "HEAD"

makeInitialCommit :: MonadGitLg m => m LG.Commit
makeInitialCommit = do
    sig <- liftIO getSig
    oid <- createTree $ return ()
    createCommit [] oid sig sig "initial (empty) commit" (Just "HEAD")

ensureHasCommit :: MonadGitLg m => m ()
ensureHasCommit = do
    hasCommit <- hasInitialCommit
    unless hasCommit $ void makeInitialCommit

getLatestCommit :: MonadGitLg m => m LG.CommitOid
getLatestCommit = do
    Just ref <- Git.resolveReference "HEAD"
    return $ Tagged ref

getLatestTree :: MonadGitLg m => m LG.TreeOid
getLatestTree = commitTree <$> (lookupCommit =<< getLatestCommit)

makeBlob :: (Pathish p, Blobbish b) => p -> b -> IO ()
makeBlob path contents = withRepo $ do
    ensureHasCommit
    parent <- getLatestCommit

    sig <- liftIO getSig
    liftIO $ print sig
    blobOid <- makeBlobOid contents
    tree <- lookupTree =<< getLatestTree
    oid <- mutateTree tree (putBlob (makeBS path) blobOid)

    commit <- createCommit [parent] oid sig sig "automagic commit"
                           (Just "HEAD")

    -- let decodeThisIsFIngPointless = Right . B8.unpack
    --     path' = B8.unpack $ makeBS path
    -- oid' <- lookupTree oid
    -- doIt $ checkoutFiles path' oid' decodeThisIsFIngPointless False

    -- ^ the proper solution doesn't work. let's hack around it.
    -- by the way, we're just trying to make sure everything is checked out
    -- so we can access it
    liftIO $ createProcess (proc "git" ["reset", "--hard"]) { cwd = Just "repo" }

    return ()

-- :(
getBlob :: BS.ByteString -> IO (Maybe BS.ByteString)
getBlob path = withRepo $ do
    ref <- Git.resolveReference "HEAD"
    case ref of
        Nothing -> return Nothing
        Just ref' -> do
            parent <- lookupCommit $ Tagged ref'
            entry <- commitTreeEntry parent path
            case entry of
                Just (BlobEntry oid kind) -> Just <$> catBlob oid
                _ -> return Nothing
