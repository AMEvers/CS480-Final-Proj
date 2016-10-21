module Proj where

import Data.Graph
import Data.Array
import Data.List

--data Tree a = Tree [Tree a] | Leaf a
--  deriving (Ord, Eq, Show, Read, Foldable)


data Color = Red | Blue | Green | Yellow
  deriving (Eq, Show, Read)

data State = AL | AK | AZ | AR | CA | CO | CT | DE | DC | FL | GA | HI | ID | IL | IN | IA | KS | KY | LA | ME | MD | MA | MI | MN | MS | MO | MT | NE| NV | NH | NJ | NM | NY | NC | ND | OH | OK | OR | PA | RI | SC | SD | TN | TX | UT | VT | VA | WA | WV | WI | WY 
  deriving (Ord, Eq, Show, Read, Enum)

startAmericaGraphFuncs :: (Graph, Vertex -> (Maybe Color, State, [State]), State -> Maybe Vertex)
startAmericaGraphFuncs = graphFromEdges $ zipWith (\x y -> (Nothing,x,y)) [(AL)..(WY)] stateConnections

--
--vertColor :: Graph -> Vertex -> Maybe Color
--vertColor g v =
-- - ()


--(vertColor, vertState, vertAdj) = \v g -> v ! g
  
(americaGraph, americaVertexInfo, americaStateLookup) = startAmericaGraphFuncs

updateFunc g = 

stateConnections :: [[State]]
stateConnections = [[GA,FL,MS,TN], [], [CA,NV,UT,CO,NM], [TX,OK,MO,TN,MS,LA], [OR,NV,AZ] ,[NM,AZ,UT,WY,NE,KS,OK], [RI,NY,MA], [MD,PA,NJ], [MD,VA], [GA,AL], [SC,NC,TN,AL,FL], [], [WA,OR,NV,UT,WY,MT], [WI,IA,MO,KY,IN], [IL,KY,OH,MI], [MN,SD,NE,MO,IL,WI], [CO,OK,MO,NE], [MO,TN,VA,WV,OH,IN,IL], [TX,AR,MS], [NH], [DE,PA,WV,DC,VA], [RI,CT,NY,VT,NH], [OH,IN,WI], [WI,IA,SD,ND], [LA,AR,TN,AL], [KS,NE,IA,IL,KY,TN,AR,OK], [ID,WY,SD,ND], [WY,SD,IA,MO,KS,CO], [CA,OR,ID,UT, AZ], [ME,MA,VT], [NY,PA,DE], [AZ,UT,CO,OK,TX], [PA,NJ,CT,MA,VT], [VA,TN,GA,SC], [MT,SD,MN], [PA,WV,KY,IN,MI], [TX,NM,CO,KS,MO,AR], [WA,ID,NV,CA], [NY,NJ,DE,MD,WV,OH], [CT,MA], [GA,NC], [WY,MT,ND,MN,IA,NE], [AR,MO,KY,VA,NC,GA,AL,MS], [NM,OK,AR,LA], [CO,NM,AZ,NV,ID,WY], [NY,MA,NH], [NC,TN,KY,WV,MD,DC], [ID,OR], [KY,OH,PA,MD,VA], [MN,IA,IL,MI], [ID,MT,SD,NE,CO , UT]]


  {--
main :: IO ()
main = undefined
--}

colorTree :: Tree a -> Tree a
colorTree = undefined

pruneNodes :: Graph -> Graph
pruneNodes x = x // newList
  where newList = filter (\(_,y) -> length y > 1) (assocs x)

adjacentNodes :: Vertex -> [State]
adjacentNodes v = adj
  where (_,_,adj) = americaVertexInfo v

degree :: Vertex -> Int
degree v = length $ adjacentNodes v

degreecmp :: Vertex -> Vertex -> Ordering
degreecmp a b = compare (degree b) (degree a)

  
deleteVert :: Vertex -> Graph -> Graph
deleteVert v g = g // map (\(x,y) -> if x == v
                                     then (x,[])
                                     else (x,(delete x y))) (assocs g)

vertexToState :: Vertex -> State
vertexToState v = vi
  where (_,vi,_) = americaVertexInfo v 

ga :: Graph -> [State]
ga g = filter (\x -> case americaStateLookup x of
                       Nothing -> False
                       x       -> degree (fromJust x) > 0) (ga' g [])

ga' :: Graph -> [Vertex] -> [State]
ga' g ignore
  | vtr == Nothing  = []
  | otherwise       = (vertexToState (fromJust vtr)) : (ga' (pruneNodes (deleteVert (fromJust vtr) g)) ((fromJust vtr):ignore))
  where vtr = gaNextToRemove g ignore

gaNextToRemove :: Graph -> [Vertex] -> Maybe Vertex
gaNextToRemove g ignore = case filter (\x -> not (elem x ignore))
                               (sortBy degreecmp
                                (map fst (assocs (pruneNodes g))))
                          of
                            [] -> Nothing
                            x  -> Just (head x) 
                            

fromJust :: Maybe a -> a
fromJust Nothing = error "oops"
fromJust (Just x) = x
