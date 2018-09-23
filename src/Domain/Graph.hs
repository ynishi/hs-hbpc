module Domain.Graph
  ( e
  , v1
  , v2
  , v12e1
  , v12
  ) where

import           Algebra.Graph

e :: Graph ()
e = empty

v1 :: Graph Int
v1 = vertex 1

v2 :: Graph Int
v2 = vertex 2

v12e1 :: Graph Int
v12e1 = connect v1 v2

v12 :: Graph Int
v12 = overlay v1 v2
