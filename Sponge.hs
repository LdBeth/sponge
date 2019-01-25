{-# LANGUAGE LambdaCase #-}

module Sponge where

import           Control.Exception        (bracket)
import           Control.Monad            (when)
import           Data.Maybe               (fromMaybe)

import           System.Environment.Blank (getEnv)
import           System.Exit              (ExitCode (ExitFailure), exitSuccess,
                                           exitWith)

import           System.Directory         (doesFileExist, removeFile)
import           System.IO                (Handle, IOMode (WriteMode), hClose,
                                           hGetBuf, hPutBuf, hPutStr, withFile,
                                           openTempFile, stdin, stdout)

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
                  (\(_,hd) -> f hd)
  where acquire = tmpdir >>= (`openTempFile` "sponge.tmp")
        finalize (path, hd) = hClose hd >> g path >> cleanUp path

renameToFile :: FilePath -> FilePath -> IO ()
renameToFile = undefined

type Source = Either FilePath CharBuffer
newtype State = State { tmpUsed :: Bool }

collectInput :: IO Source
collectInput = do buf <- newCharBuffer bufSize WriteBuffer
                  n <- withBuffer buf writeBuf
                  if n < bufSize
                    then return $ Right buf
                    else undefined
                    where writeBuf x = hGetBuf stdin x bufSize

writeOp :: String -> IO ()
writeOp = putStr

castOutput :: Maybe FilePath -> Source -> IO ()

castOutput = f
  where f (Just x) s = do test <- doesFileExist x
                          if test
                            then undefined
                            else withFile x WriteMode (\x -> writeTo x s)
        f Nothing s = writeTo stdout s
        writeTo h = \case
          Left f  -> readFile f >>= hPutStr h
          Right b -> withBuffer b
                     (\x -> hPutBuf h x bufSize)

sponge :: [String] -> IO ()
sponge args = do out <- parseArg args
                 input <- collectInput
                 castOutput out input

-- >>> hgetbuffering stdout

