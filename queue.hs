import qualified Data.Sequence as Seq
import Data.IORef


data Queue a = Queue{queue :: IORef(Seq.Seq a)}

initQueue :: IO(Queue a)
initQueue = Queue <$> (newIORef(Seq.empty) :: IO(IORef(Seq.Seq a)))

pushQ :: (Queue a) -> a -> IO(Queue a)
pushQ que x = do
    modifyIORef'(queue que) (Seq.|> x)
    return que

popQ :: (Queue a) -> IO()
popQ que = modifyIORef'(queue que)(seqTail.Seq.viewl)
        where
            seqTail(a Seq.:< b) = b
backQ :: (Queue a) -> IO (Maybe a)
backQ que = do
    q <- readIORef (queue que)
    case (q Seq.!? ((Seq.length q) - 1) ) of
        Just x -> return (Just x)
        Nothing -> return(Nothing)

frontQ :: (Queue a) -> IO (Maybe a)
frontQ que = do
    q <- readIORef (queue que)
    case (q Seq.!? 0) of
        Just x -> return (Just x)
        Nothing -> return(Nothing)

emptyQ :: Queue a -> IO Bool
emptyQ que = do
    q <- readIORef (queue que)
    if(Seq.null q) then pure True
    else pure False

sizeQ :: (Queue a) ->IO Int
sizeQ que = Seq.length <$> readIORef(queue que)
    

main = do
    n <- initQueue
    pushQ n 5
    pushQ n 100
    popQ n
    popQ n
    backQ n
    pushQ n 100
    pushQ n 65
    print =<< backQ n
    print =<< frontQ n
    print =<< sizeQ n
    
{-
Just 65
Just 100
2
-}
