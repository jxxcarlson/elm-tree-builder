module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tree
import Tree.Build as Build
import Tree.Extra
import Tree.Graph
import Tree.Random
import Tree.Svg
import Tree.Transform exposing (defaults)


type alias Model =
    { sourceText : String
    , windowHeight : Int
    , windowWidth : Int
    , message : String
    , seed : Random.Seed
    , lineNumber : Int
    , graph : Result Build.Error Tree.Graph.Graph
    , tree : Result Build.Error (Tree.Tree String)
    , labelStyle : Tree.Svg.LabelStyle
    , aperture : Float
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { width : Int, height : Int, seed : Int }



-- Parameters


initialAperture =
    0.25


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialGraphString
      , windowHeight = flags.height
      , windowWidth = flags.width
      , seed = Random.initialSeed flags.seed |> Debug.log "SEED2"
      , message = ""
      , lineNumber = 0
      , graph = Result.map (Tree.Transform.toGraph (preferences initialAperture) identity) (Build.fromString "?" identity initialGraphString)
      , tree = Build.fromString "?" identity initialGraphString
      , labelStyle = Tree.Svg.NoLabel
      , aperture = initialAperture
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | InputText String
    | ClearText
    | TreeRequested
    | TreeSelected File
    | TreeLoaded String
    | SaveToFile
    | RandomTree
    | SetLabelStyle Tree.Svg.LabelStyle


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
                , graph = Result.map (Tree.Transform.toGraph (preferences model.aperture) identity) tree_
              }
            , Cmd.none
            )

        ClearText ->
            ( { model
                | sourceText = ""
              }
            , Cmd.none
            )

        TreeRequested ->
            ( model
            , Select.file [ "text/plain" ] TreeSelected
            )

        TreeSelected file ->
            ( model
            , Task.perform TreeLoaded (File.toString file)
            )

        TreeLoaded content ->
            ( { model
                | sourceText = content
                , graph = Result.map (Tree.Transform.toGraph (preferences model.aperture) identity) (Build.fromString "?" identity content)
                , tree = Build.fromString "?" identity content
              }
            , Cmd.none
            )

        SaveToFile ->
            ( model, download model.sourceText )

        RandomTree ->
            let
                ( numberOfNodes, seed_ ) =
                    Random.step (Random.int 3 40) model.seed

                ( randomNumber, seed ) =
                    Random.step (Random.int 0 numberOfNodes) seed_

                sourceText =
                    Tree.Random.generateOutline numberOfNodes randomNumber

                tree =
                    Build.fromString "?" identity sourceText

                graph =
                    Result.map (Tree.Transform.toGraph (preferences model.aperture) identity) tree
            in
            ( { model | sourceText = sourceText, tree = tree, graph = graph, seed = seed }, Cmd.none )

        SetLabelStyle labelStyle ->
            ( { model | labelStyle = labelStyle }, Cmd.none )


download : String -> Cmd msg
download treeData =
    Download.string "tree.txt" "text/plain" treeData


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


preferences aperture =
    { defaults | ballRadius = 10, halfAngle = aperture * pi, scaleFactor = 0.8 }


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
            ++ Tree.Svg.render model.labelStyle (Tree.Svg.transform 280 100 60 60 0.5 graph_)
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
        [ column [ centerY, paddingEach { top = 16, bottom = 0, left = 0, right = 0 }, Element.spacing 8, Element.width (px appWidth_), Element.height (px (appHeight_ model)) ]
            [ title "Tree Test App"
            , row [ Element.spacing 12, Element.width (px appWidth_) ]
                [ openFileButton
                , saveToFileButton
                , randomTreeButton
                , row [ Element.spacing 6, alignRight ]
                    [ labelStyleButton Tree.Svg.NoLabel
                    , labelStyleButton Tree.Svg.FullLabel
                    , labelStyleButton Tree.Svg.FirstWord
                    ]
                ]
            , column [ Element.height (panelHeight model), Element.spacing 12 ]
                [ row [] [ editor model, rhs model ]
                ]
            , row
                [ Element.paddingXY 12 0
                , Element.height (px 30)
                , Element.width Element.fill
                , Font.size 14
                , Background.color (Element.rgb 0.3 0.3 0.3)
                , Font.color (Element.rgb 1 1 1)
                , Element.spacing 12
                ]
                [ Element.text ("Nodes = " ++ (Result.map (Tree.Extra.nodeCount >> String.fromInt) model.tree |> Result.withDefault "?"))
                , Element.text ("Depth = " ++ (Result.map (Tree.Extra.depth >> String.fromInt) model.tree |> Result.withDefault "?"))
                ]
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, fontGray 0.9 ] [ Element.text str ]



-- BUTTONS


defaultButtonColor =
    Element.rgb255 60 60 60


labelStyleButton labelStyle =
    Element.Input.button buttonStyle
        { onPress = Just (SetLabelStyle labelStyle)
        , label = el [ centerX, centerY, Font.size 14 ] (Element.text (labelStyleToString labelStyle))
        }


labelStyleToString : Tree.Svg.LabelStyle -> String
labelStyleToString labelStyle =
    case labelStyle of
        Tree.Svg.NoLabel ->
            "No Label"

        Tree.Svg.FullLabel ->
            "Full Label"

        Tree.Svg.FirstWord ->
            "First Word"


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


randomTreeButton : Element Msg
randomTreeButton =
    Element.Input.button buttonStyle
        { onPress = Just RandomTree
        , label = el [ centerX, centerY, Font.size 14 ] (Element.text "Random Tree")
        }


openFileButton : Element Msg
openFileButton =
    Element.Input.button buttonStyle
        { onPress = Just TreeRequested
        , label = el [ centerX, centerY, Font.size 14 ] (Element.text "Open File")
        }


saveToFileButton : Element Msg
saveToFileButton =
    Element.Input.button buttonStyle
        { onPress = Just SaveToFile
        , label = el [ centerX, centerY, Font.size 14 ] (Element.text "Save to File")
        }


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , Background.color (rgb255 60 60 60)
    , paddingXY 15 8
    , mouseDown [ Background.color (rgb255 180 180 255) ]
    ]



-- STYLE


grayColor g =
    Element.rgb g g g


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
