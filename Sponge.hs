{-# LANGUAGE LambdaCase #-}

module Sponge where

import           Control.Exception        (bracket, finally)
import           Control.Monad            (when, (>=>))
import           Data.Maybe               (fromMaybe)

import           System.Environment.Blank (getEnv)
import           System.Exit              (ExitCode (ExitFailure), exitSuccess,
                                           exitWith)

import           System.Directory         (doesFileExist, removeFile,
                                           renameFile)
import           System.IO                (Handle, IOMode (WriteMode), hClose,
                                           hGetBuf, hPutBuf, hPutStr,
                                           openTempFile, stdin, stdout,
                                           withFile)

-- import System.Posix.Files (isRegularFile)

import           GHC.IO.Buffer            (BufferState (ReadBuffer, WriteBuffer),
                                           CharBuffer, newCharBuffer,
                                           withBuffer)
-- What ever it is
bufSize :: Int
bufSize = 8192 -- 5

tmpdir :: IO FilePath
tmpdir = fromMaybe "/tmp/" <$> getEnv "TMPDIR"

usage :: IO ()
usage = putStrLn
        "sponge <file>: soak up all input from stdin and write it to <file>"

parseArg :: [String] -> IO (Maybe FilePath)
parseArg = \case
               ["-h"] -> usage >> exitSuccess
               [a]    -> return $ Just a
               []     -> return Nothing
               _      -> usage >> exitWith (ExitFailure 1)

cleanUp :: FilePath -> IO ()
cleanUp f = do exist <- doesFileExist f
               when exist $ removeFile f

withTmpFile :: (Handle -> IO ()) -> (FilePath -> IO ()) -> IO ()
withTmpFile f g = bracket acquire
                  finalize
                  (\(path,hd) -> f hd `finally` hClose hd >> g path)
  where acquire = tmpdir >>= (`openTempFile` "sponge.tmp")
        finalize (path, _) =  cleanUp path

type Source = Either ((FilePath -> IO ()) -> IO ()) CharBuffer
newtype State = State { tmpUsed :: Bool }

putBuf h b = withBuffer b $ \x -> hPutBuf h x bufSize

collectInput :: IO Source
collectInput = do buf <- newCharBuffer bufSize WriteBuffer
                  n <- withBuffer buf writeBuf
                  return (if n < bufSize
                           then Right buf
                           else Left $ tmpFile buf)
                    where writeBuf x = hGetBuf stdin x bufSize
                          tmpFile buf = withTmpFile
                                         (\h -> do putBuf h buf
                                                   str <- getContents
                                                   hPutStr h str)
                          -- TODO: test this

castOutput :: Maybe FilePath -> Source -> IO ()
castOutput = f
  where f (Just x) s = do test <- doesFileExist x
                          if test || case s of
                                       Right _ -> True
                                       _       -> False
                            then withFile x WriteMode (`writeTo` s)
                            else (\(Left p) -> p (`renameFile` x)) s
        f Nothing s = writeTo stdout s
        writeTo h = \case
          Left f  -> f $ readFile >=> hPutStr h
          Right b -> putBuf h b

sponge :: [String] -> IO ()
sponge args = do out <- parseArg args
                 input <- collectInput
                 castOutput out input
-- >>> hgetbuffering stdout

