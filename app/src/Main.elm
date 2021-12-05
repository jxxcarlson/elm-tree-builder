module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree
import Tree.Build as Build
import Tree.Graph
import Tree.Svg
import Tree.Transform exposing (defaults)
import Tree.TransformR as TR


type alias Model =
    { sourceText : String
    , windowHeight : Int
    , windowWidth : Int
    , message : String
    , lineNumber : Int
    , graph : Result Build.Error Tree.Graph.Graph
    , tree : Result Build.Error (Tree.Tree String)
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialGraphString
      , windowHeight = flags.height
      , windowWidth = flags.width
      , message = ""
      , lineNumber = 0
      , graph = Result.map (Tree.Transform.toGraph preferences identity) (Build.fromString "?" identity initialGraphString)
      , tree = Build.fromString "?" identity initialGraphString
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | InputText String
    | ClearText


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText t ->
            let
                tree_ =
                    Build.fromString "?" identity t
            in
            ( { model
                | sourceText = t
                , tree = tree_
                , graph = Result.map (Tree.Transform.toGraph preferences identity) tree_
              }
            , Cmd.none
            )

        ClearText ->
            ( { model
                | sourceText = ""
              }
            , Cmd.none
            )


tree1 : Result Build.Error (Tree.Tree String)
tree1 =
    Build.fromString "?" identity forestData


forestData =
    """
*
  1
    2
    3
  4
    5
    6
"""


forestData1 =
    """
*
 1
  2
  3
 4
  5
  6
"""


initialGraphString =
    """
1
 2
  3
   4
   5
  6
 7
"""


str2 =
    """
1
 2
  3
   4
   5
  6
 7
  8
   9
   10
   11
"""


outline =
    """
1 Home
  2 Important things (according to Cicero)
    3 Library
    4 Garden
      5 Pond
      6 Grasses
      7 Flowers
"""


tree2 =
    Build.fromString "?" identity "1\n 2\n  3\n  4\n"



--\n 3\n  6\n   10\n   11"


graph =
    Result.map (Tree.Transform.toGraph preferences identity) tree1 |> Result.withDefault []


graph1 =
    Result.map (TR.toGraph 3 identity) tree1 |> Result.withDefault []


preferences =
    { defaults | ballRadius = 10, halfAngle = 0.1 * pi, scaleFactor = 0.85 }


render : Model -> Tree.Graph.Graph -> Html msg
render model graph_ =
    let
        h =
            String.fromInt <| rawPanelHeight model - 24
    in
    svg
        [ Svg.Attributes.width "900"
        , Svg.Attributes.height "900"
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt panelWidth_ ++ h)
        , Svg.Attributes.fill "white"
        ]
        ([ rect
            [ Svg.Attributes.x "10"
            , Svg.Attributes.y "10"
            , Svg.Attributes.width (String.fromInt panelWidth_)
            , Svg.Attributes.height h
            , Svg.Attributes.rx "15"
            , Svg.Attributes.ry "15"
            , Svg.Attributes.fill "white"
            ]
            []
         ]
            ++ Tree.Svg.render Tree.Svg.FullLabel (Tree.Svg.transform 280 100 60 60 0.5 graph_)
        )


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


mainColumnStyle model =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    , Element.width (px (appWidth_ + 40))
    , Element.height (px (appHeight_ model + 40))
    ]


rhs : Model -> Element Msg
rhs model =
    let
        toRender =
            case model.graph of
                Ok g ->
                    [ render model g |> Element.html ]

                Err error ->
                    [ Element.text "No valid graph" ]
    in
    column [ Element.spacing 8 ]
        [ row
            [ fontGray 0.9
            , Element.spacing 12
            , moveDown 20
            , Font.size 14
            ]
            toRender
        ]


editor_ : Model -> Element Msg
editor_ model =
    let
        onChange : Html.Attribute Msg
        onChange =
            Json.Decode.string
                |> Json.Decode.at [ "target", "editorText" ]
                |> Json.Decode.map InputText
                |> Html.Events.on "change"
    in
    el [ htmlAttribute onChange ] <|
        html <|
            Html.node "ace-editor"
                [ HtmlAttr.attribute "theme" "twilight"
                , HtmlAttr.attribute "wrapmode" "true"
                , HtmlAttr.attribute "tabsize" "2"
                , HtmlAttr.attribute "linenumber" (String.fromInt (model.lineNumber + 1))
                , HtmlAttr.attribute "softtabs" "true"
                , HtmlAttr.attribute "navigateWithinSoftTabs" "true"
                , HtmlAttr.attribute "fontsize" "12"
                , HtmlAttr.style "height" (String.fromInt (innerPanelHeight model) ++ "px")
                , HtmlAttr.style "width" (String.fromInt panelWidth_ ++ "px")
                , HtmlAttr.attribute "text" model.sourceText
                ]
                []



-- PARAMETERS


widePanelWidth_ =
    2 * panelWidth_


panelWidth_ =
    560


appHeight_ model =
    model.windowHeight - 140


rawPanelHeight model =
    appHeight_ model - 160


panelHeight model =
    px (rawPanelHeight model)


innerPanelHeight model =
    appHeight_ model - 180


appWidth_ =
    2 * panelWidth_ + 15


editor model =
    column [ Element.height (px (innerPanelHeight model)), moveUp 28 ]
        [ row [ Element.spacing 12 ]
            []
        , editor_ model
        ]


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column (mainColumnStyle model)
        [ column [ centerY, paddingEach { top = 46, bottom = 0, left = 0, right = 0 }, Element.spacing 8, Element.width (px appWidth_), Element.height (px (appHeight_ model)) ]
            [ -- title "L3 Demo App"
              column [ Element.height (panelHeight model), Element.spacing 12 ]
                [ row [] [ editor model, rhs model ]
                ]
            , row [ Element.paddingXY 8 0, Element.height (px 30), Element.width Element.fill, Font.size 14, Background.color (Element.rgb 0.3 0.3 0.3), Font.color (Element.rgb 1 1 1) ] [ Element.text model.message ]
            ]
        ]



-- STYLE


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
