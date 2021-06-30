module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Color
import Css
import Css.Global
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
        , Css.backgroundColor (Css.rgb 225 225 20)
        ]
    , Css.Global.id "editor-main"
        [ Css.position Css.relative
        , Css.fontFamily Css.monospace
        , Css.whiteSpace Css.pre
        , Css.overflowX Css.visible
        , Css.overflowY Css.visible
        ]
    , Css.Global.id "editor-main-inner"
        [ Css.outline Css.none
        ]
    , Css.Global.id "content-main"
        [ Css.position Css.absolute
        , Css.property "user-select" "none"
        , Css.em 1 |> Css.padding
        , Css.outline3 (Css.px 0) Css.solid Css.transparent
        , Css.property "user-select" "text"
        , Css.property "-moz-user-select" "text"
        , Css.property "-webkit-user-select" "text"
        , Css.property "-ms-user-select" "text"
        , Css.backgroundColor (Css.rgb 200 200 20)
        , Css.px config.lineHeight |> Css.lineHeight
        , Css.px config.fontSize |> Css.fontSize
        ]
    , Css.Global.class "content-line"
        [ Css.position Css.absolute
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px config.lineHeight |> Css.lineHeight
        , Css.px config.lineHeight |> Css.height
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
        , editableTextForeignObject frame
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


editableTextForeignObject : Sized a -> Svg Msg
editableTextForeignObject _ =
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
    H.div
        [ HA.id "content-main"
        , HA.contenteditable True
        ]
        [ H.node "elm-editable"
            [ HA.attribute "spellcheck" "false"
            , HA.attribute "autocorrect" "off"
            , HA.attribute "autocapitalize" "off"
            ]
            [ H.div [] [ H.text "Editable Post-It Note" ]
            , H.node "selection-handler"
                []
                []
            ]
        ]
