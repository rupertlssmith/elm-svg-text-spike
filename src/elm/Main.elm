module Main exposing (main)

-- import Arc2d exposing (Arc2d)
-- import Browser
-- import Browser.Dom exposing (Viewport, getViewport)
-- import Browser.Events exposing (onResize)
-- import Color exposing (Color)
-- import Curve2d exposing (Curve2d)
-- import Ease
-- import Geometry.Svg
-- import Html exposing (Html)
-- import Html.Attributes
-- import Html.Lazy
-- import Http
-- import Json.Decode as Decode exposing (Decoder)
-- import Json.Decode.Extra exposing (andMap, withDefault)
-- import LineSegment2d exposing (LineSegment2d)
-- import Point2d exposing (Point2d)
-- import Ports.SVGTextPort exposing (textToSVG, textToSVGResponse)
-- import Task exposing (Task, perform)
-- import TeaTree exposing (Tree, Zipper)
-- import TextToSVG exposing (TextAlignment(..), TextRenderFunc, textAsPath, textAsText)
-- import TypedSvg exposing (circle, g, line, path, rect, svg, text_, tspan)
-- import TypedSvg.Attributes
--     exposing
--         ( color
--         , d
--         , fill
--         , fillOpacity
--         , fontFamily
--         , preserveAspectRatio
--         , shapeRendering
--         , stroke
--         , strokeDasharray
--         , strokeLinecap
--         , strokeLinejoin
--         , textAnchor
--         , textRendering
--         , transform
--         , viewBox
--         )
-- import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
-- import TypedSvg.Core exposing (Svg, svgNamespace, text)
-- import TypedSvg.Events
-- import TypedSvg.Types
--     exposing
--         ( Align(..)
--         , AnchorAlignment(..)
--         , Fill(..)
--         , MeetOrSlice(..)
--         , Opacity(..)
--         , Scale(..)
--         , ShapeRendering(..)
--         , StrokeLinecap(..)
--         , StrokeLinejoin(..)
--         , TextRendering(..)
--         , Transform(..)
--         , px
--         )

import Utils.GridMetrics exposing (Frame, Size, Sized, middle, rectToFrame)



-- import Vector2d exposing (Vector2d)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = SizingWindowModel
    | Ready ReadyModel


type alias ReadyModel =
    { frame : Frame
    }


type Msg
    = WindowSize Size


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindowModel
    , Task.perform (viewportToSize >> WindowSize) getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize coordsToSize |> Sub.map WindowSize


noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model, action ) of
        ( SizingWindow sizingWindowModel, WindowSize windowSize ) ->
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
        |> rectToFrame



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
    svg
        [ preserveAspectRatio (Align ScaleMid ScaleMid) Meet
        , viewBox (round frame.x |> toFloat)
            (round frame.y |> toFloat)
            (round frame.w |> toFloat)
            (round frame.h |> toFloat)
        , svgNamespace
        , shapeRendering RenderGeometricPrecision
        ]
        [ background frame ]


background : Sized a -> Svg msg
background size =
    let
        skirtScale =
            10
    in
    rect
        [ fill <| Fill offWhite
        , fillOpacity <| Opacity 0.8
        , strokeWidth 0
        , x -(skirtScale * size.w)
        , y -(skirtScale * size.h)
        , width <| (2 * skirtScale + 1) * size.w
        , height <| (2 * skirtScale + 1) * size.h
        ]
        []
