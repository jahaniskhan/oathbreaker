module Utils.DataStructures
    ( CircularBuffer
    , newCircularBuffer
    , addToBuffer
    , getBufferContents
    ) where

import Data.IORef

-- | Circular buffer data structure
data CircularBuffer a = CircularBuffer
    { buffer  :: IORef [a]
    , maxSize :: Int
    }

-- | Create a new circular buffer
newCircularBuffer :: Int -> IO (CircularBuffer a)
newCircularBuffer size = do
    ref <- newIORef []
    return $ CircularBuffer ref size

-- | Add an element to the circular buffer
addToBuffer :: CircularBuffer a -> a -> IO ()
addToBuffer cb element = do
    buf <- readIORef (buffer cb)
    let newBuf = take (maxSize cb) (element : buf)
    writeIORef (buffer cb) newBuf

-- | Get the contents of the circular buffer
getBufferContents :: CircularBuffer a -> IO [a]
getBufferContents cb = readIORef (buffer cb)
