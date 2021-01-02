module Data.RoundRobin
    ( RoundRobin
    , newRoundRobin
    , select
    , set
    ) where
import Data.Array.IO
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))

newtype RoundRobin a = RoundRobin (IORef [a]) deriving Eq

newRoundRobin :: NonEmpty a -> IO (RoundRobin a)
newRoundRobin (x :| xs) =
    newIORef (cycle (x:xs)) >>= return . RoundRobin

select :: RoundRobin a -> IO a
select (RoundRobin ref) = atomicModifyIORef' ref (\ (a:as) -> (as, a))

set :: RoundRobin a -> NonEmpty a -> IO ()
set (RoundRobin ref) (x :| xs) = atomicModifyIORef' ref (const (cycle (x:xs) , ()))

main :: IO ()
main = do 
    reqs <- mapM HTTP.parseUrl ["http://foo.com", "http://bar.com", "http://qux.com"]
    proxyTable <- RR.newRoundRobin reqs
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    req <- RR.select proxyTable
    res <- HTTP.httpLbs req manager