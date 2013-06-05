module IRDeadcode (optimize) where

import qualified IR
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List
import qualified Data.Maybe

type VertexMap = Map.Map Graph.Vertex IR.BasicBlock
type LabelMap = Map.Map IR.Label Graph.Vertex
type EdgeMap = Map.Map Graph.Vertex (Set.Set Graph.Vertex)

optimize :: IR.Program [IR.BasicBlock] -> IR.Program [IR.BasicBlock]
optimize (fs, gs) = (map optimizeFunc fs, gs)

optimizeFunc :: IR.IRFunc [IR.BasicBlock] -> IR.IRFunc [IR.BasicBlock]
optimizeFunc (IR.Func l ts bbs rt) = IR.Func l ts (optimizeBBs bbs) rt

optimizeBBs :: [IR.BasicBlock] -> [IR.BasicBlock]
optimizeBBs bbs = snd $ unzip $ flip filter (Map.assocs vm) $ \(v, _) -> elem v reachables -- Really use assocs to return bbs in correct order (= ascending keys)
	where
		vm = makeVertexMap bbs
		lm = makeLabelMap vm
		em = makeEdgeMap vm lm
		g = makeGraph (0, length bbs) em
		reachables = Graph.reachable g 0

makeVertexMap :: [IR.BasicBlock] -> VertexMap
makeVertexMap bbs = Map.fromList $ zip [0..] bbs

makeLabelMap :: VertexMap -> LabelMap
makeLabelMap vm = Map.fromList $ flip map (Map.toList vm) $ \(v, bb) ->
	case Data.List.head bb of
		IR.Label str -> (str, v)

makeEdgeList :: EdgeMap -> [Graph.Edge]
makeEdgeList em = concat $ flip map (Map.toList em) $ \(x, ys) -> flip map (Set.toList ys) $ \y -> (x, y)

makeGraph :: Graph.Bounds -> EdgeMap -> Graph.Graph
makeGraph b em = Graph.buildG b $ makeEdgeList em

makeEdgeMap :: VertexMap -> LabelMap -> EdgeMap
makeEdgeMap vm lm = Map.fromList $ flip map (Map.toList vm) $ \(v, bb) -> (v, Set.fromList $ map toVertex $ usedLabels bb)
	where
		toVertex :: IR.Label -> Graph.Vertex
		toVertex l = case Map.lookup l lm of
			Just v -> v
			Nothing -> if l == IR.errorLabel then 0 else error $ "Can not find label " ++ l
		
		usedLabels :: IR.BasicBlock -> [IR.Label]
		usedLabels bb = concat $ flip map bb $ \stmt -> case stmt of
			IR.Jump x	-> [x]
			IR.CJump _ x y	-> [x, y]
			_		-> []
