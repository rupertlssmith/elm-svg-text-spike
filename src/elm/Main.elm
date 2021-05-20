module Main exposing (main)

-- import Geometry.Svg
-- import LineSegment2d exposing (LineSegment2d)
-- import Point2d exposing (Point2d)
-- import TextToSVG exposing (TextAlignment(..), TextRenderFunc, textAsPath, textAsText)
-- import Vector2d exposing (Vector2d)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Color exposing (Color)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InEm as InEm
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types as SvgTypes
    exposing
        ( Align(..)
        , AnchorAlignment(..)
        , MeetOrSlice(..)
        , Opacity(..)
        , Paint(..)
        , Scale(..)
        , ShapeRendering(..)
        , StrokeLinecap(..)
        , StrokeLinejoin(..)
        , TextRendering(..)
        , Transform(..)
        )
import Utils.GridMetrics as GridMetrics exposing (Frame, Size, Sized)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = SizingWindow
    | Ready ReadyModel


type alias ReadyModel =
    { frame : Frame
    }


type Msg
    = WindowSize Size
    | EditorChange EditorChangeEvent
    | SelectionChange SelectionChangeEvent


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize coordsToSize |> Sub.map WindowSize


noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, Debug.log "msg" msg ) of
        ( SizingWindow, WindowSize windowSize ) ->
            noop (Ready { frame = windowSizeToFrame windowSize })

        ( _, _ ) ->
            noop model


coordsToSize : Int -> Int -> Size
coordsToSize x y =
    { w = toFloat x, h = toFloat y }


viewportToSize : Viewport -> Size
viewportToSize vport =
    { w = vport.viewport.width, h = vport.viewport.height }


windowSizeToFrame : Size -> Frame
windowSizeToFrame size =
    { x = 0.0, y = 0.0, w = size.w, h = size.h }
        |> GridMetrics.rectToFrame



-- Rendering


black =
    Color.black


white =
    Color.white


offWhite =
    Color.rgb 242 235 238


midGray =
    Color.gray


strongPrintGray =
    Color.rgb 32 32 32


printGray =
    Color.rgb 48 48 48


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Text Editing Example"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    H.div []
        [ Html.Lazy.lazy fullBody model
        ]


fullBody : Model -> Html Msg
fullBody model =
    case model of
        Ready ready ->
            H.div
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "overflow" "hidden"
                ]
                [ editableContent ready
                ]

        _ ->
            H.div [] []


diagram : { frame : Frame } -> Html Msg
diagram diag =
    let
        frame =
            diag.frame
    in
    Svg.svg
        [ SvgAttr.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
        , SvgAttr.viewBox (round frame.x |> toFloat)
            (round frame.y |> toFloat)
            (round frame.w |> toFloat)
            (round frame.h |> toFloat)
        , SvgCore.svgNamespace
        , SvgAttr.shapeRendering RenderGeometricPrecision
        ]
        [ background frame
        , editableText frame
        ]


background : Sized a -> Svg msg
background size =
    let
        skirtScale =
            10
    in
    Svg.rect
        [ SvgAttr.fill <| Paint offWhite
        , SvgAttr.fillOpacity <| Opacity 0.8
        , InPx.strokeWidth 0
        , InPx.x -(skirtScale * size.w)
        , InPx.y -(skirtScale * size.h)
        , InPx.width <| (2 * skirtScale + 1) * size.w
        , InPx.height <| (2 * skirtScale + 1) * size.h
        ]
        []


editableText : Sized a -> Svg msg
editableText size =
    Svg.text_
        [ InPx.x 20, InEm.y 2 ]
        [ Svg.tspan [ InPx.x 0, InPx.dy 20 ]
            [ H.text "editable line1" ]
        , Svg.tspan [ InPx.x 0, InPx.dy 20 ]
            [ H.text "editable line2" ]
        ]



-- contenteditable stuff


editableContent : ReadyModel -> Html Msg
editableContent ready =
    -- let
    --     cursor =
    --         model.controlCursor
    -- in
    H.div
        [ HA.id "content-main"

        --, HA.style "height" (String.fromFloat model.bufferHeight ++ "px")
        , HA.contenteditable True
        ]
        [ -- viewCursors model,
          H.node "elm-editable"
            [ HE.on "textchange" editorChangeDecoder
            , HE.on "selectionchange" selectionChangeDecoder
            , HA.attribute "spellcheck" "false"
            , HA.attribute "autocorrect" "off"
            , HA.attribute "autocapitalize" "off"

            -- , HE.on "beforeinput" beforeInputDecoder
            -- , HE.preventDefaultOn "cut" (( Cut (), True ) |> Decode.succeed)
            -- , HE.preventDefaultOn "copy" (( Copy (), True ) |> Decode.succeed)
            -- , HE.on "pastewithdata" pasteWithDataDecoder
            ]
            [ diagram ready
            , H.node "selection-handler"
                [-- cursorToSelection model.controlCursor model.startLine model.buffer
                 --     |> selectionEncoder model.startLine
                 --     |> HA.property "selection"
                ]
                []
            ]
        ]



-- Browser selections.


type Selection
    = NoSelection
    | Collapsed
        { offset : Int
        , node : Path
        }
    | Range
        { anchorOffset : Int
        , anchorNode : Path
        , focusOffset : Int
        , focusNode : Path
        }


type alias Path =
    List Int


selectionDecoder : Decode.Decoder Selection
selectionDecoder =
    let
        range aNode aOffset fNode fOffset =
            Range
                { anchorOffset = aOffset
                , anchorNode = aNode
                , focusOffset = fOffset
                , focusNode = fNode
                }

        collapsed fNode fOffset =
            Collapsed
                { offset = fOffset
                , node = fNode
                }
    in
    Decode.at [ "selection" ] Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "collapsed" ->
                        Decode.succeed collapsed
                            |> andMap (Decode.at [ "node" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "offset" ] Decode.int)

                    "range" ->
                        Decode.succeed range
                            |> andMap (Decode.at [ "anchorNode" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "anchorOffset" ] Decode.int)
                            |> andMap (Decode.at [ "focusNode" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "focusOffset" ] Decode.int)

                    _ ->
                        Decode.succeed NoSelection
            )


selectionEncoder : Int -> Selection -> Encode.Value
selectionEncoder startLine sel =
    case sel of
        NoSelection ->
            [ ( "selection", Encode.string "noselection" )
            , ( "startLine", Encode.int startLine )
            ]
                |> Encode.object

        Collapsed val ->
            [ ( "selection", Encode.string "collapsed" )
            , ( "startLine", Encode.int startLine )
            , ( "node", Encode.list Encode.int val.node )
            , ( "offset", Encode.int val.offset )
            ]
                |> Encode.object

        Range val ->
            [ ( "selection", Encode.string "range" )
            , ( "startLine", Encode.int startLine )
            , ( "anchorNode", Encode.list Encode.int val.anchorNode )
            , ( "anchorOffset", Encode.int val.anchorOffset )
            , ( "focusNode", Encode.list Encode.int val.focusNode )
            , ( "focusOffset", Encode.int val.focusOffset )
            ]
                |> Encode.object



-- Selection change events.


type alias SelectionChangeEvent =
    { selection : Selection
    , isControl : Bool
    , timestamp : Int
    }


selectionChangeDecoder : Decode.Decoder Msg
selectionChangeDecoder =
    Decode.succeed SelectionChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "ctrlEvent" ] Decode.bool)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)
        |> Decode.map SelectionChange



-- Editor mutation events.


type alias EditorChangeEvent =
    { selection : Selection
    , characterDataMutations : List TextChange
    , timestamp : Int
    , isComposing : Bool
    }


type alias TextChange =
    ( Path, String )


editorChangeDecoder : Decode.Decoder Msg
editorChangeDecoder =
    Decode.succeed EditorChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)
        |> andMap (Decode.at [ "detail", "isComposing" ] (Decode.oneOf [ Decode.bool, Decode.succeed False ]))
        |> Decode.map EditorChange


characterDataMutationsDecoder : Decode.Decoder (List TextChange)
characterDataMutationsDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "path" (Decode.list Decode.int))
            (Decode.field "text" Decode.string)
        )



-- Helpers


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
