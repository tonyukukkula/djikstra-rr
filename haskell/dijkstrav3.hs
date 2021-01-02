import Data.Array.*
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Set as S
import System.IO
import Control.*
import Data.*
 
dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v,w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do
  previous <- newSTArray b invalid_index
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, w) = do
                  let dist_thru_u = dist + w
                  old_dist <- readArray min_distance
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance v dist_thru_u
                    writeArray previous v u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  p <- freeze previous
  m <- freeze min_distance
  return (m, p)
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray
 
shortest_path_to :: (Ix v) => v -> v -> Array v v -> [v]
shortest_path_to target invalid_index previous =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (previous ! vertex) (vertex : acc)
 
adj_list :: Array Char [(Char, Int)]
adj_list = listArray ('a', 'f') [ [('b',7), ('c',9), ('f',14)],
                                  [('a',7), ('c',10), ('d',15)],
                                  [('a',9), ('b',10), ('d',11), ('f',2)],
                                  [('b',15), ('c',11), ('e',6)],
                                  [('d',6), ('f',9)],
                                  [('a',14), ('c',2), ('e',9)] ]
 
main :: IO ()
main = do
  let (min_distance, previous) = dijkstra 'a' ' ' adj_list
  putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
  let path = shortest_path_to 'e' ' ' previous
  putStrLn $ "Path: " ++ show path