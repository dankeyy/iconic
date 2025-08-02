import qualified Data.Map.Strict as M
import Data.Function ((&))
import Data.List (find, nub)
import Data.Maybe (fromMaybe)


data Node = Lam | App | Dup | Root | Aux | Erase
  deriving (Show, Eq)


-- 1 principle port 2 auxiliary ports (P1, P2)
data Port = PR
          | P1
          | P2
  deriving (Show, Eq, Ord)


type NodeId = Int
type Plug = (NodeId, Port)


data Net = Net
  { nodes  :: M.Map NodeId Node
  , wires  :: M.Map Plug Plug
  , nextId :: NodeId
  }
  deriving (Show)


-- net utils
emptyNet :: Net
emptyNet = Net M.empty M.empty 0


addNode :: Node -> Net -> (NodeId, Net)
addNode n net =
  let i = nextId net
  in (i, net { nodes = M.insert i n (nodes net), nextId = i + 1 })


connect :: Plug -> Plug -> Net -> Net
connect a b net =
  -- disconnect old connections first to keep symmetry
  let net' = disconnect a $ disconnect b net
  -- connection is always bidirectional
  in net' { wires = M.insert a b $ M.insert b a (wires net') }


disconnect :: Plug -> Net -> Net
disconnect p net = case M.lookup p (wires net) of
  Nothing -> net
  Just q  -> net { wires = M.delete p $ M.delete q (wires net) }


wireNeighbor :: Plug -> Net -> Maybe Plug
wireNeighbor p net = M.lookup p (wires net)


removeNodes :: [NodeId] -> Net -> Net
removeNodes ns net = net { nodes = foldr M.delete (nodes net) ns }


removeWires :: [NodeId] -> Net -> Net
removeWires ns net = net { wires = M.filterWithKey (\(nid, _) p -> nid `notElem` ns && fst p `notElem` ns) (wires net) }


connectMaybe :: Plug -> Maybe Plug -> Net -> Net
connectMaybe _ Nothing = id
connectMaybe fixed (Just var) = connect fixed var


-- same as connectMaybe but for two maybe plugs
connectMaybes :: Maybe Plug -> Maybe Plug -> Net -> Net
connectMaybes (Just a) (Just b) = connect a b
connectMaybes _ _ = id



-- actual interaction logic
step :: Net -> Maybe Net
step net = case find isActivePair netWires of
  Just ((i, _), (j, _)) -> case (n1, n2) of
    (Lam, App) -> Just $ annihilate i j net
    (App, Lam) -> Just $ annihilate j i net
    (Dup, Dup) -> Just $ annihilate i j net

    (Dup, Lam) -> Just $ duplicate i j net
    (Lam, Dup) -> Just $ duplicate j i net
    (Dup, App) -> Just $ duplicate i j net
    (App, Dup) -> Just $ duplicate j i net

    _          -> Nothing
    where
      n1 = nodes net M.! i
      n2 = nodes net M.! j
  Nothing -> Nothing
  where
    netWires = M.toList (wires net)
    isActivePair ((x, px), (y, py)) = px == PR && py == PR && x /= y


hasLoopOn :: NodeId -> Net -> Bool
hasLoopOn k net = case (wireNeighbor (k, P1) net, wireNeighbor (k, P2) net) of
  (Just (k', P2), Just (k'', P1)) | k' == k && k'' == k -> True
  _ -> False


annihilate :: NodeId -> NodeId -> Net -> Net
annihilate i j net =
  let
    [p1i, p2i] = map (\p -> wireNeighbor (i, p) net) [P1, P2]
    [p1j, p2j] = map (\p -> wireNeighbor (j, p) net) [P1, P2]

    loopOnI = hasLoopOn i net
    loopOnJ = hasLoopOn j net

    cleanedNet = net
               -- disconnect principal
               & disconnect (i, PR)
               & disconnect (j, PR)
               -- disconnect aux
               & disconnect (i, P1)
               & disconnect (i, P2)
               & disconnect (j, P1)
               & disconnect (j, P2)
               -- cleanup nodes and wires from the graph
               & removeWires [i, j]
               & removeNodes [i, j]
  in
    case (loopOnI, loopOnJ) of
      (True, False) -> connectMaybes p1j p2j cleanedNet
      (False, True) -> connectMaybes p1i p2i cleanedNet
      (True, True) -> cleanedNet
      (False, False) -> cleanedNet
                      & connectMaybes p1i p1j
                      & connectMaybes p2i p2j


duplicate :: NodeId -> NodeId -> Net -> Net
duplicate dupId constId net =
  let
    constType = nodes net M.! constId
    [out1, out2] = map (\p -> wireNeighbor (dupId, p) net) [P1, P2]
    [sub1, sub2] = map (\p -> wireNeighbor (constId, p) net) [P1, P2]

    cleanedNet = net
               -- disconnect principal
               & disconnect (dupId, PR)
               & disconnect (constId, PR)
               -- disconnect aux
               & disconnect (dupId, P1)
               & disconnect (dupId, P2)
               & disconnect (constId, P1)
               & disconnect (constId, P2)
               -- cleanup nodes and wires from the graph
               & removeWires [dupId, constId]
               & removeNodes [dupId, constId]

    -- add new consts
    (c1, n1) = addNode constType cleanedNet
    (c2, n2) = addNode constType n1

    -- connect the previous wirings from dup to the new consts
    netWithConsts = n2
                  & connectMaybe (c1, PR) out1
                  & connectMaybe (c2, PR) out2
  in
    if hasLoopOn constId net then
        -- we've reached something like the identity function
        -- we connect the aux ports and wrap up
        -- (no point adding dups)
        netWithConsts
          & connect (c1, P1) (c1, P2)
          & connect (c2, P1) (c2, P2)
      else
        let
          -- make new dups
          (d1, n3) = addNode Dup netWithConsts
          (d2, n4) = addNode Dup n3
        in n4
          -- connect consts to dups at the aux ports
          & connect (c1, P1) (d1, P1)
          & connect (c2, P1) (d1, P2)
          & connect (c1, P2) (d2, P1)
          & connect (c2, P2) (d2, P2)
          -- connect the previous wirings from const to the new dups
          & connectMaybe (d1, PR) sub1
          & connectMaybe (d2, PR) sub2


eval :: Net -> IO Net
eval net = case step net of
  Nothing -> return net
  Just net' -> do
    putStrLn "\nStep result:"
    printNet net'
    eval net'


printNet :: Net -> IO ()
printNet net = do
  putStrLn "Nodes:"
  mapM_ print (M.toList (nodes net))
  putStrLn "Wires:"
  mapM_ print (M.toList (wires net))


writeDot :: FilePath -> Net -> IO ()
writeDot path net = writeFile path $ toDot net


toDot :: Net -> String
toDot net =
  "graph Net {\n" ++
  concatMap nodeLine (M.toList $ nodes net) ++
  concatMap edgeLine (undirectedEdges $ wires net) ++
  "}\n"
  where
    nodeLine (i, n) =
      show i ++ " [label=\"" ++ show n ++ "\\n" ++ show i ++ "\"];\n"
    undirectedEdges ws =
      [((a,ap),(b,bp)) | ((a,ap),(b,bp)) <- M.toList ws, (a,ap) < (b,bp)]
    edgeLine ((i,pi),(j,pj)) =
      "  " ++ show i ++ " -- " ++ show j ++
      " [label=\"" ++ show pi ++ "<->" ++ show pj ++ "\"];\n"


prettyNet :: Net -> String
prettyNet net = unlines $
  [ show i ++ " " ++ show n ++ ":"
      ++ concat [ "  " ++ show p ++ "→" ++
          maybe "-" (show . fst) (wireNeighbor (i,p) net)
        | p <- [PR, P1, P2] ]
  | (i,n) <- M.toList (nodes net)
  ]


main :: IO ()
main = do
  let (lam, n0)   = addNode Lam emptyNet
  let (dup, n1)   = addNode Dup n0
  let (app, n2)   = addNode App n1
  let n3          = connect (lam, P1) (dup, PR) n2
  let n4          = connect (dup, P1) (app, PR) n3
  let n5          = connect (dup, P2) (app, P1) n4
  let n6          = connect (lam, P2) (app, P2) n5

  let (lam1, n7)  = addNode Lam n6
  let n8          = connect (lam1, P1) (lam1, P2) n7

  let (app1, n11) = addNode App n8
  let n12         = connect (app1, PR) (lam, PR) n11
  let n13         = connect (app1, P1) (lam1, PR) n12

  let (root, n17) = addNode Root n13
  let netInitial  = connect (root, PR) (app1, P2) n17

  putStrLn "Initial net:"
  printNet netInitial

  putStrLn "\nStarting reduction steps:"

  netFinal <- eval netInitial

  writeDot "net0.dot" netInitial
  writeDot "netFinal.dot" netFinal
  putStrLn "Visualize with: dot -Tpng net0.dot -o net0.png && dot -Tpng netFinal.dot -o netFinal.png"

  putStrLn "\nAfter reduction (final net):"
  printNet netFinal
