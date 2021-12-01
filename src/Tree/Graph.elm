module Tree.Graph exposing (Edge, Graph, Node)


type alias Graph =
    List Edge


type alias Edge =
    { from : Node, to : Node, color : String }


type alias Node =
    { name : String, x : Float, y : Float, r : Float, color : String }
