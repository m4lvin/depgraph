module DAGViz (defaultDotC2) where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing (renderDot)
import Data.Text.Lazy (pack, unpack)

toDotString :: DotGraph Node -> String
toDotString dg = unpack $ renderDot $ toDot dg

makeClusterParams2 :: (Show el) => (Node -> nl -> String) -> (Node -> nl -> Maybe String) -> GraphvizParams Node nl el String nl
makeClusterParams2 f g =
  defaultParams
    { isDotCluster = idc,
      clusterBy = cb,
      clusterID = Str . pack,
      fmtNode = \(xn, xl) -> [(Label . StrLabel . pack) (f xn xl)],
      fmtEdge = const [] -- no annotation on edges
    }
  where
    idc _ = True
    cb (xn, xl) = case g xn xl of
      Just xc -> C xc (N (xn, xl))
      Nothing -> N (xn, xl)

defaultVisC2 :: (Show el) => (Node -> nl -> String) -> (Node -> nl -> Maybe String) -> Gr nl el -> DotGraph Node
defaultVisC2 f g = graphToDot (makeClusterParams2 f g)

defaultDotC2 :: (Show el) => (Node -> nl -> String) -> (Node -> nl -> Maybe String) -> Gr nl el -> String
defaultDotC2 f g graph = toDotString (defaultVisC2 f g graph)
