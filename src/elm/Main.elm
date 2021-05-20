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
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Task exposing (Task)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
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
update action model =
    case ( model, action ) of
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
    Html.div []
        [ Html.Lazy.lazy fullBody model
        ]


fullBody : Model -> Html Msg
fullBody model =
    case model of
        Ready ready ->
            Html.div
                [ Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "overflow" "hidden"
                ]
                [ diagram ready
                ]

        _ ->
            Html.div [] []


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
        [ background frame ]


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
