module Tree.Example.String2 exposing (..)

import Tree.Build as Build exposing (InitialData)
import Tree.Render as Render


initialData : InitialData String
initialData =
    { quant = 1
    , defaultNode = "?"
    , rootNode = "root"
    , makeNode = identity
    }


tree =
    Build.fromString initialData data


testResult =
    Render.test 1 identity initialData data


data =
    """1
 2
  3
4"""
