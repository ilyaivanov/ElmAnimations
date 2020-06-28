module ExtraEvents exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


type alias MouseMoveEvent =
    { pageX : Int
    , pageY : Int
    , layerX : Int
    , layerY : Int
    , buttons : Int
    }


type alias Offsets =
    { offsetX : Int
    , offsetY : Int
    }


type alias MouseDownEvent =
    { offsets : Offsets
    , mousePosition : MouseMoveEvent
    }


onMouseMove : (MouseMoveEvent -> msg) -> Attribute msg
onMouseMove tagger =
    on "mousemove" (Json.map tagger mouseMoveDecoder)


onMouseDown : (MouseDownEvent -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Json.map tagger mouseDownDecoder)



--noinspection ALL


onMouseDownAlwaysStopPropagation : msg -> Attribute msg
onMouseDownAlwaysStopPropagation msg =
    stopPropagationOn "mousedown" (Json.map alwaysStop (Json.succeed msg))


onMouseUp : msg -> Attribute msg
onMouseUp tagger =
    on "mouseup" (Json.succeed tagger)



--noinspection ALL


onMouseEnter : msg -> Attribute msg
onMouseEnter tagger =
    on "mouseenter" (Json.succeed tagger)


mouseDownDecoder : Json.Decoder MouseDownEvent
mouseDownDecoder =
    Json.map2 MouseDownEvent offsetsDecoder mouseMoveDecoder


mouseMoveDecoder : Json.Decoder MouseMoveEvent
mouseMoveDecoder =
    Json.map5 MouseMoveEvent
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)
        (Json.field "layerX" Json.int)
        (Json.field "layerY" Json.int)
        (Json.field "buttons" Json.int)


offsetsDecoder : Json.Decoder Offsets
offsetsDecoder =
    Json.map2 Offsets
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)



--noinspection ALL


onKeyUp : (String -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger (Json.field "key" Json.string))



--noinspection ALL


onClickAlwaysStopPropagation : msg -> Attribute msg
onClickAlwaysStopPropagation msg =
    stopPropagationOn "click" (Json.map alwaysStop (Json.succeed msg))


alwaysStop x =
    ( x, True )



--noinspection ALL


onClickIf : Bool -> msg -> Attribute msg
onClickIf condition msg =
    if condition then
        onClick msg

    else
        emptyAttribute


classIf : Bool -> String -> Attribute msg
classIf condition className =
    if condition then
        class className

    else
        emptyAttribute


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        emptyAttribute


elementIf : Bool -> Html msg -> Html msg
elementIf condition element =
    if condition then
        element

    else
        emptyElement


emptyAttribute =
    style "" ""


emptyElement =
    text ""
