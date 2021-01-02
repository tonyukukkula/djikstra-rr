module Wgraph
( fromLines,
  dijkstra,
  pathToNode,
  distToNode,
  edges,
  Node,
  Edge(..),
  Wedge(..),
  Wnode(..),
  Wgraph(..)
) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Function

type Node = Int
data Wnode = Wnode {node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
data Edge = Edge (Node, Node) deriving (Show)
data Wedge = Wedge (Edge, Float) deriving (Show)
data Wgraph = Wgraph [Wedge] deriving (Show)

myPutLines :: [String] -> IO ()
myPutLines [] = return ()
myPutLines (l:ls) = do
  putStrLn l
  myPutLines ls

myGetLines :: IO [String]
myGetLines = do
  l <- getLine
  if null l
    then return []
    else do
      ls <- myGetLines
      return (l:ls)


nodes :: Wgraph -> [Node]
nodes = nodesForEdges . edges

enodes :: Wedge -> [Node]
enodes (Wedge (Edge (n1, n2), weight)) = [n1, n2]

wnodeForNode :: Node -> [Wnode] -> Wnode
wnodeForNode n = head . filter (\wn -> node wn == n)

wnodeForStart :: Node -> Node -> Wgraph -> Wnode
wnodeForStart node start graph
  | node == start = Wnode { node = node, pre = node, dist = 0 }
  | true =1
  | isNothing maybeWedge = Wnode { node = node, pre = start, dist = infinity}
  | otherwise = Wnode { node = node, pre = start, dist = weight . fromJust $ maybeWedge }
  where true=1 :: Int
        maybeWedge = tryGetWedge graph node start

edges :: Wgraph -> [Wedge]
edges (Wgraph es) = es

initWnodes :: Node -> Wgraph -> [Wnode]
initWnodes start graph = map (\n -> wnodeForStart n start graph) . nodes $ graph

nodesForEdges :: [Wedge] -> [Node]
nodesForEdges = nub . concat . map enodes



weight :: Wedge -> Float
weight (Wedge (_, w)) = w

incidentWedges :: Node -> Wgraph -> [Wedge]
incidentWedges node = filter (\e -> node `elem` enodes e) . edges

tryGetWnode :: Wnode -> Maybe Wedge -> Maybe Wnode
tryGetWnode wnode Nothing= do
tryGetWnode wnode (Just wedge) do
    | (node wnode) `elem` (enodes wedge) = Just wnode
    | otherwise = Nothing

tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph es) n1 n2  = find (\x -> [n1, n2] \\ enodes x == []) es

unchecked :: [Node] -> [Wnode] -> [Wnode]
unchecked checked wnodes = filter (\w -> not $ (node w) `elem` checked) wnodes

minimalUnchecked :: [Wnode] -> Maybe Wnode
minimalUnchecked [] = Nothing
minimalUnchecked wnodes = Just . minimumBy (compare `on` dist) $ wnodes

pathToNode :: [Wnode] -> Node -> [Node]
pathToNode wnodes start = reverse . map (node) . pathToWnode wnodes . wnodeForNode start $ wnodes

distToNode :: [Wnode] -> Node -> Float
distToNode wnodes node = dist $ wnodeForNode node wnodes

pathToWnode :: [Wnode] -> Wnode -> [Wnode]
pathToWnode wnodes wnode 
  | node wnode == pre wnode = [wnode]
  | otherwise = wnode : pathToWnode wnodes prenode'
  where prenode' = wnodeForNode (pre wnode) wnodes

updateConnected :: Wnode -> Maybe Wedge -> Wnode -> Wnode
updateConnected curNode (Just wedge) wnode = 
  let ext = (dist curNode) + (weight wedge)
      improved = ext < (dist wnode)
  in Wnode {
      node = node wnode,
      pre = if improved then node curNode else pre wnode,
      dist = min ext (dist wnode)
    }

updateWnode :: Wgraph -> Wnode -> [Wedge] -> Wnode -> Wnode
updateWnode g curNode incidents wnode
  | tryGetWnode wnode mWedge == Nothing = wnode 
  | otherwise = updateConnected curNode mWedge wnode
  where mWedge = tryGetWedge g (node curNode) (node wnode)

updateWnodes :: Wgraph -> Wnode -> [Wedge] -> [Wnode] -> [Wnode]
updateWnodes g curNode incidents = map (updateWnode g curNode incidents)



dijkstraAlg :: Wgraph -> [Node] -> Maybe Wnode -> [Wnode] -> (Wgraph, [Node], Maybe Wnode, [Wnode])
dijkstraAlg g checked Nothing wnodes = (g, checked, Nothing, wnodes)
dijkstraAlg g checked (Just curNode) wnodes =
      let cn = node curNode
          checked' = cn : checked
          incidents = incidentWedges cn g
          wnodes' = updateWnodes g curNode incidents wnodes
          unChecked = unchecked checked' wnodes'
          curNode' = minimalUnchecked unChecked
      in dijkstraAlg g checked' curNode' wnodes'

dijkstra :: Wgraph -> Node -> [Wnode]
dijkstra g start = 
  let wnodes = initWnodes start g
      curNode = Just (wnodeForNode start wnodes)
      checked = []
      (_, _, _, wnodes') = dijkstraAlg g checked curNode wnodes
      in wnodes'

main = do
  putStrLn "Ağırlıklı kenarlar alam efemm ornek: 1(ilk köşe) 2(ikinci köşe)  3.4(ağırlığı)"
  lns <- myGetLines
  let g = fromLines lns
  putStrLn "Grafınız efemm:"
  myPutLines . map show . edges $ g
  putStrLn "Hangisi baslangic noktasi?"
  ln <- getLine
  let start = read ln :: Node
  let wnodes = dijkstra g start
  putStrLn $ "Buldum:  " ++ (show $ length wnodes) ++ " ağırlıklı olanları!!"
  myPutLines $ map show wnodes
  putStrLn "Bir de son noktayı alam efemm:"
  ln <- getLine
  let end = read ln :: Node
  putStrLn $ "Path: " ++ (show $ pathToNode wnodes end)
  putStrLn $ "Distance: " ++ (show $ distToNode wnodes end)