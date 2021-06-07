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
import Css
import Css.Global
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Html.Styled
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


config =
    let
        fontSize =
            30

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeightRatio = lineHeightRatio
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , lineLength = 120
    , numLines = 10000
    , blinkInterval = 400
    }


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
    | InputMsg InputEvent
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | PageUp
    | PageDown
    | LineHome
    | LineEnd
    | FileHome
    | FileEnd
    | RemoveCharBefore
    | RemoveCharAfter
    | NewLine
    | NoOp


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
    case ( model, msg ) of
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



-- Styling


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


global : List Css.Global.Snippet
global =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height ]
    , Css.Global.id "post-it-note"
        [ Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height

        --, Css.em 1 |> Css.padding
        , Css.backgroundColor (Css.rgb 225 225 20)
        ]
    , Css.Global.id "editor-main"
        [ Css.position Css.relative
        , Css.fontFamily Css.monospace
        , Css.whiteSpace Css.pre
        , Css.overflowX Css.visible
        , Css.overflowY Css.visible

        --, Css.pct 100 |> Css.width
        --, Css.pct 100 |> Css.height
        ]
    , Css.Global.id "editor-main-inner"
        [ -- Css.displayFlex
          -- , Css.flexDirection Css.row,
          Css.outline Css.none
        ]
    , Css.Global.id "content-main"
        [ Css.position Css.absolute

        --, Css.property "flex" "1"
        , Css.property "user-select" "none"
        , Css.em 1 |> Css.padding
        , Css.outline3 (Css.px 0) Css.solid Css.transparent
        , Css.property "user-select" "text"
        , Css.property "-moz-user-select" "text"
        , Css.property "-webkit-user-select" "text"
        , Css.property "-ms-user-select" "text"
        , Css.backgroundColor (Css.rgb 200 200 20)
        , Css.whiteSpace Css.noWrap
        , Css.px config.lineHeight |> Css.lineHeight
        , Css.px config.fontSize |> Css.fontSize

        -- , Css.px config.lineHeight |> Css.lineHeight
        -- , Css.px config.lineHeight |> Css.height
        --, Css.property "caret-color" "transparent"
        ]
    , Css.Global.class "content-line"
        [ Css.position Css.absolute
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px config.lineHeight |> Css.lineHeight
        , Css.px config.lineHeight |> Css.height

        --, Css.backgroundColor (Css.rgb 190 195 167)
        ]
    , Css.Global.class "cursors"
        [ Css.position Css.relative
        ]
    , Css.Global.class "cursor"
        [ Css.position Css.absolute
        , Css.px config.lineHeight |> Css.height
        , Css.borderLeft3 (Css.px 2.5) Css.solid (Css.rgb 90 95 167)
        ]
    , Css.Global.selector "::selection"
        [ Css.backgroundColor (Css.rgb 196 195 217)
        ]
    ]



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Text Editing Example"
    , body =
        [ Css.Global.global global |> Html.Styled.toUnstyled
        , body model
        ]
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
                [ diagram ready
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


editableText : Sized a -> Svg Msg
editableText size =
    SvgCore.foreignObject
        [ InPx.x 50
        , InPx.y 50
        , InPx.width 400
        , InPx.height 400
        ]
        [ H.div [ HA.id "post-it-note" ]
            [ editableContent
            ]
        ]



-- contenteditable stuff


editableContent : Html Msg
editableContent =
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
            [ HE.on "textchange" (editorChangeDecoder |> Decode.map EditorChange)
            , HE.on "selectionchange" (selectionChangeDecoder |> Decode.map SelectionChange)
            , HA.attribute "spellcheck" "false"
            , HA.attribute "autocorrect" "off"
            , HA.attribute "autocapitalize" "off"
            , HE.on "beforeinput" (beforeInputDecoder |> Decode.map InputMsg)

            -- , HE.preventDefaultOn "cut" (( Cut (), True ) |> Decode.succeed)
            -- , HE.preventDefaultOn "copy" (( Copy (), True ) |> Decode.succeed)
            -- , HE.on "pastewithdata" pasteWithDataDecoder
            ]
            [ H.div [] [ H.text "Editable Post-It Note" ]
            , H.node "selection-handler"
                [-- cursorToSelection model.controlCursor model.startLine model.buffer
                 --     |> selectionEncoder model.startLine
                 --     |> HA.property "selection"
                ]
                []
            ]
        ]



-- Cursor model.


type Cursor
    = NoCursor
    | ActiveCursor RowCol
    | RegionCursor
        { -- The control cursor, always clipped to the virtual scroll window.
          start : RowCol
        , end : RowCol

        -- The selection within the text model.
        , selectionStart : RowCol
        , selectionEnd : RowCol
        }


type alias RowCol =
    { row : Int
    , col : Int
    }



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


selectionChangeDecoder : Decode.Decoder SelectionChangeEvent
selectionChangeDecoder =
    Decode.succeed SelectionChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "ctrlEvent" ] Decode.bool)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)



-- Editor mutation events.


type alias EditorChangeEvent =
    { selection : Selection
    , characterDataMutations : List TextChange
    , timestamp : Int
    , isComposing : Bool
    }


type alias TextChange =
    ( Path, String )


editorChangeDecoder : Decode.Decoder EditorChangeEvent
editorChangeDecoder =
    Decode.succeed EditorChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)
        |> andMap (Decode.at [ "detail", "isComposing" ] (Decode.oneOf [ Decode.bool, Decode.succeed False ]))


characterDataMutationsDecoder : Decode.Decoder (List TextChange)
characterDataMutationsDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "path" (Decode.list Decode.int))
            (Decode.field "text" Decode.string)
        )



-- Editor input events.


type alias InputEvent =
    { data : Maybe String
    , isComposing : Bool
    , inputType : String
    }


beforeInputDecoder : Decoder InputEvent
beforeInputDecoder =
    Decode.succeed InputEvent
        |> andMap (Decode.maybe (Decode.field "data" Decode.string))
        |> andMap (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
        |> andMap (Decode.field "inputType" Decode.string)



-- Keyboard events.


type alias KeyEvent =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    , isComposing : Bool
    }


keyMsgDecoder : Decoder ( Msg, Bool )
keyMsgDecoder =
    Decode.succeed KeyEvent
        |> andMap (Decode.field "keyCode" Decode.int)
        |> andMap (Decode.field "key" Decode.string)
        |> andMap (Decode.field "altKey" Decode.bool)
        |> andMap (Decode.field "metaKey" Decode.bool)
        |> andMap (Decode.field "ctrlKey" Decode.bool)
        |> andMap (Decode.field "shiftKey" Decode.bool)
        |> andMap (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
        |> Decode.andThen keyToMsg


keyToMsg : KeyEvent -> Decoder ( Msg, Bool )
keyToMsg keyEvent =
    case keyEvent.key of
        "ArrowUp" ->
            Decode.succeed ( MoveUp, True )

        "ArrowDown" ->
            Decode.succeed ( MoveDown, True )

        "ArrowLeft" ->
            Decode.succeed ( MoveLeft, True )

        "ArrowRight" ->
            Decode.succeed ( MoveRight, True )

        "PageUp" ->
            Decode.succeed ( PageUp, True )

        "PageDown" ->
            Decode.succeed ( PageDown, True )

        "Backspace" ->
            Decode.succeed ( RemoveCharBefore, True )

        "Delete" ->
            Decode.succeed ( RemoveCharAfter, True )

        "Enter" ->
            Decode.succeed ( NewLine, True )

        "Home" ->
            if keyEvent.ctrlKey then
                Decode.succeed ( FileHome, True )

            else
                Decode.succeed ( LineHome, True )

        "End" ->
            if keyEvent.ctrlKey then
                Decode.succeed ( FileEnd, True )

            else
                Decode.succeed ( LineEnd, True )

        _ ->
            Decode.succeed ( NoOp, False )



-- Helpers


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
