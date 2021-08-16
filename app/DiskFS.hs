{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Simon Zeng, 2021
-- License     :  AGPL v3
--
-- Maintainer  :  contact@simonzeng.com
-- Stability   :  experimental
-- Portability :  portable
--
-- You have an ADT for a hard drive (black blox implementation):
--
-- > class Disk:
-- >   - int length ()
-- >   - int current_position ()
-- >   - char read()
-- >   - void write(char ch)
--
-- and then an ADT for a file system on top of the hard drive:
--
-- > class FileSystem(Disk d)
-- >   - void create_file(string filename, string contents)
-- >   - Read_handle read_file(string filename)
-- >   - void on_disk_spin()
--
-- You can save file metadata in ram.
-- You don't control when or to where the disk spins -- the file system calls the @on_disk_spin@ callback whenever the disk spins.
--
-- First, implement @create_file@ and @on_disk_spin@. e.g. running:
--
-- > create_file("foo", "bar")
-- > create_file("a", "b")
-- > create_file("baz", "bar")
--
-- should give a disk that (eventually, w/ enough spins) has:
--
-- +---------+----------+
-- | filename| contents |
-- +---------+----------+
-- |   foo   |    bar   |
-- +---------+----------+
-- |   a     |    b     |
-- +---------+----------+
-- |   baz   |    bar   |
-- +---------+----------+
--
-- Make sure that your data is set up in a way that allows @read_file@ to be implemented
--
-- To extend: You have a mutable struct @Read_handle@:
--
-- > Read_handle
-- >  - bool done
-- >  - string contents
-- >  - ...
--
-- The @done@ field should be set to true whenever the @contents@ field has all of the contents from the file
--
-- Task: implement @read_file@, and modify @on_disk_spin@ to populate read handles
module Main where

import Control.Monad (MonadPlus, ap, guard, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Strict
  ( MonadState,
    get,
    modify',
    put,
  )
import Control.Monad.Trans.Cont (callCC, evalContT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import Data.Foldable (traverse_)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Map.Strict (Map, maxViewWithKey)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (read, readFile)

-- | The provided disk api from the question, translated to haskell
class Disk a where
  dlength :: a -> Int
  current_position :: a -> IO Int
  read :: a -> IO Char
  write :: a -> Char -> IO ()

-- | Store contents of disks and files as Map instead of array for convenience. perf optimization: replace with array
newtype ContiguousData = ContiguousData {cData :: Map Int Char} deriving (Eq, Ord, Semigroup, Monoid)

-- | Change of basis from Map Int Char to ContiguousData
-- TODO: auto derive this w/ @Coercible@
cMap :: (Map Int Char -> Map Int Char) -> ContiguousData -> ContiguousData
cMap f = ContiguousData . f . cData

instance Show ContiguousData where
  show (ContiguousData d) = show [fromMaybe '_' (Map.lookup i d) | i <- [0..highestIndex]]
    where
      highestIndex = case maxViewWithKey d of
        Nothing -> 0
        (Just ((i, _), _)) -> i


-- | File metadata struct
data File = File
  { name :: String,
    position :: Int,
    size :: Int,
    unwrittenContents :: ContiguousData
  }
  deriving (Eq, Ord, Show)

type FileName = String

-- | Struct to hold mapping of disk position -> file at that position
type ReverseLookup = Map Int File

-- | Struct for filesystem metadata
data FileSystem = FileSystem
  { -- | Set of files on disk
    files :: Set File,
    -- | Map of disk position -> file
    freverseLookup :: ReverseLookup,
    -- | Index of lowest free position (we don't support delete so no fragmentation)
    freeIndex :: Int,
    -- | Map of filename to mutable read handles
    filesToRead :: Map String (IORef ReadHandle)
  }

-- | Helper function to make code more understandable for imperative people
continue :: Applicative f => f ()
continue = pure ()

-- | `createFile` has access to an environment that has access to a disk, and returns a nullable FileSystem
createFile ::
  (Monad m, Disk a, MonadReader a m, MonadPlus m) =>
  FileSystem ->
  String ->
  String ->
  m FileSystem
createFile (FileSystem files rl freeIndex ftr) filename fcontents = do
  disk <- ask -- ask for disk from environment
  let contentsMap = ContiguousData $ Map.fromList (zip [0 ..] fcontents)
      newFile = File filename freeIndex (length fcontents) contentsMap

  -- guard: if any bool fails, return Nothing
  guard (position newFile + size newFile <= dlength disk)
  guard (0 <= position newFile + size newFile)
  guard (not (Set.member filename $ Set.map name files))

  let newFiles = Set.insert newFile files
      newRl = foldl (\rlAccum index -> Map.insert index newFile rlAccum) rl [freeIndex, freeIndex + 1 .. newFreeIndex - 1]
      newFreeIndex = freeIndex + length fcontents

  return (FileSystem newFiles newRl newFreeIndex ftr)

-- | `onDiskSpin` also has access to a disk env, and has access to IO
onDiskSpin ::
  (Monad m, Disk a, MonadReader a m, MonadIO m) =>
  FileSystem ->
  m FileSystem
onDiskSpin fs@(FileSystem files rl freeIndex readHandles) = evalContT . callCC $ \ret -> do
  disk <- ask
  curPos <- liftIO $ current_position disk

  file <- maybe (ret fs) pure (Map.lookup curPos rl) -- break early w/ callCC if there's no file at the position
  case Map.lookup (curPos - position file) (cData $ unwrittenContents file) of
    Nothing -> continue
    (Just char) -> liftIO $ write disk char

  charAtCurPos <- liftIO $ read disk
  let maybeReadHandle = Map.lookup (name file) readHandles
  liftIO $ traverse (`modifyIORef'` updateHandle file curPos charAtCurPos) maybeReadHandle -- update handle if handle is present

  let newFile = File (name file) (position file) (size file) (cMap (Map.delete (curPos - position file)) (unwrittenContents file))
      newFiles = Set.insert newFile (Set.delete file files)
      newRl = Map.insert curPos newFile (Map.delete curPos rl)
  return $ FileSystem newFiles newRl freeIndex readHandles
  where
    updateHandle :: File -> Int -> Char -> ReadHandle -> ReadHandle
    updateHandle file curPosition char handle =
      let newContents = cMap (Map.insert (curPosition - position file) char) (rcontents handle)
          ready = length (Map.toList . cData $ rcontents handle) == size file
       in ReadHandle newContents ready

-- | Immutable handle type. Usually passed in an IORef for mutability
data ReadHandle = ReadHandle {rcontents :: ContiguousData, isReady :: Bool} deriving (Show)

-- | `readFile` has an internal modifyable FileSystem state, has IO, and returns a reference to a ReadHandle
readFile ::
  (Monad m, MonadState FileSystem m, MonadIO m) =>
  String ->
  m (IORef ReadHandle)
readFile fileName = evalContT . callCC $ \ret -> do
  (FileSystem _files __rl _freeIndex filesToRead) <- get

  let oldHandle = Map.lookup fileName filesToRead
  case oldHandle of
    Nothing -> continue
    (Just h) -> ret h -- break early if handle already exists

  newHandle <- liftIO . newIORef $ ReadHandle mempty False
  let newFilesToRead = Map.insert fileName newHandle filesToRead
  modify' (\fs' -> fs' {filesToRead = newFilesToRead})

  return newHandle

-- Bonus part: implement a concrete disk implementation to test w/

-- | We create a DSL to write sequences of disk instructions in
data Simulator a where
  Bind :: Simulator a -> (a -> Simulator b) -> Simulator b
  Pure :: a -> Simulator a
  TryCreateFile :: String -> String -> Simulator ()
  SpinTo :: Int -> Simulator ()
  TryRead :: String -> Simulator ()
  PrintState :: Simulator ()

instance Monad Simulator where
  (>>=) = Bind

instance Applicative Simulator where
  pure = Pure
  (<*>) = ap

instance Functor Simulator where
  fmap = liftM

-- | Sample program using the dsl to create 3 files and try reading them at various times
sampleProgram :: Simulator ()
sampleProgram = do
  PrintState
  TryCreateFile "file1" "asdfghjkl"
  TryCreateFile "file2" "asdfghjkl"
  TryRead "file1"
  TryRead "file2"
  TryRead "file3"
  traverse_ SpinTo [0 .. 12]
  traverse_ SpinTo [12, 11 .. 0]
  TryRead "file1"
  TryRead "file2"
  TryRead "file3"
  traverse_ SpinTo [15, 17]
  TryRead "file1"
  TryRead "file2"
  TryRead "file3"
  traverse_ SpinTo [12 .. 20]
  TryRead "file1"
  TryRead "file2"
  TryRead "file3"
  TryCreateFile "file3" "nn"
  TryRead "file3"
  traverse_ SpinTo [0 .. 100]
  TryRead "file3"
  PrintState

-- | Concrete implementation of Disk
data ConcreteDisk = ConcreteDisk
  { cLength :: Int,
    cCurPos :: IORef Int,
    cStuff :: IORef ContiguousData
  }

instance Disk ConcreteDisk where
  dlength = cLength
  current_position = readIORef . cCurPos
  read d = do
    stuff <- readIORef $ cStuff d
    curPos <- readIORef $ cCurPos d
    return . fromMaybe '_' . Map.lookup curPos $ cData stuff
  write d c = do
    let stuffRef = cStuff d
    curPos <- readIORef $ cCurPos d
    modifyIORef' stuffRef (cMap $ Map.insert curPos c)

-- | Haskell can auto derive printing most data types, but not if the type is impure.
printFs :: FileSystem -> IO ()
printFs fs = do
  ftr :: Map String ReadHandle <- traverse readIORef (filesToRead fs)
  print ftr
  putStrLn $
    unwords
      [ "FileSystem",
        show $ files fs,
        show $ freverseLookup fs,
        show $ freeIndex fs,
        show ftr
      ]

-- | @Disk@ is also impure so it also needs its own print
printDisk d = do
  curPos <- readIORef $ cCurPos d
  stuff <- readIORef $ cStuff d
  putStrLn $ unwords ["Disk", show (cLength d), show curPos, show stuff]

-- | @runSimulation@ has access to a ConcreteDisk environment, a FileSystem internal state, and IO
runSimulation ::
  (Monad m, MonadReader ConcreteDisk m, MonadState FileSystem m, MonadIO m) =>
  Simulator a ->
  m a
runSimulation (TryCreateFile name contents) = do
  disk <- ask
  fs <- get
  let newFs = runReaderT (createFile fs name contents) disk
  case newFs of
    Nothing -> liftIO . putStrLn $ unwords ["Failed to create file", name, "with contents", show contents]
    (Just newFs') -> do
      liftIO . putStrLn $ unwords ["Created file", name, "with contents", show contents]
      put newFs'
runSimulation (SpinTo i) = do
  disk <- ask
  fs <- get
  let posRef = cCurPos disk
  liftIO $ writeIORef posRef i
  newFs <- liftIO $ runReaderT (onDiskSpin fs) disk
  put newFs
-- liftIO . putStrLn $ unwords ["Spun to", show i]
runSimulation (TryRead name) = do
  fs <- get
  (handleRef, newFs) <- liftIO $ runStateT (readFile name) fs
  handle <- liftIO $ readIORef handleRef
  liftIO . putStrLn $ unwords ["Reading", name, "->", show handle]
  put newFs
runSimulation PrintState = do
  disk <- ask
  fs <- get
  liftIO $ printDisk disk
  liftIO $ printFs fs
runSimulation (Pure x) = return x
runSimulation (Bind a mb) = do
  aRes <- runSimulation a
  runSimulation (mb aRes)

startingFileSystem :: FileSystem
startingFileSystem = FileSystem mempty mempty 0 mempty

mkStartingDisk :: IO ConcreteDisk
mkStartingDisk = do
  startingIndex <- newIORef 0
  startingContents <- newIORef mempty
  return $ ConcreteDisk 200 startingIndex startingContents

main :: IO ()
main = do
  startingDisk <- mkStartingDisk
  runReaderT (evalStateT (runSimulation sampleProgram) startingFileSystem) startingDisk
  -- evalStateT (runReaderT (runSimulation sampleProgram) startingDisk) startingFileSystem
