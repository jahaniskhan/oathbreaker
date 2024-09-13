module Utils.DataStructures
    ( CircularBuffer
    , newCircularBuffer
    , addToBuffer
    , getBufferContents
    ) where

import Data.IORef

-- | Circular buffer data structure
data CircularBuffer a = CircularBuffer
    { buffer :: IORef [a]
    , maxSize :: Int
    }

-- | Create a new circular buffer
-- 
-- @param size The maximum size of the buffer
-- @return The new circular buffer
newCircularBuffer :: Int -> IO (CircularBuffer a)
newCircularBuffer size = do
    ref <- newIORef []
    return $ CircularBuffer ref size

-- | Add an element to the circular buffer
-- 
-- @param buffer The circular buffer
-- @param element The element to add
addToBuffer :: CircularBuffer a -> a -> IO ()
addToBuffer (CircularBuffer ref size) element = do
    buf <- readIORef ref
    let newBuf = take size (element : buf)
    writeIORef ref newBuf

-- | Get the contents of the circular buffer
-- 
-- @param buffer The circular buffer
-- @return The contents of the buffer
getBufferContents :: CircularBuffer a -> IO [a]
getBufferContents (CircularBuffer ref _) = readIORef ref
