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
    , offsetX : Int
    , offsetY : Int
    , buttons : Int
    }


type alias Offsets =
    { offsetX : Int
    , offsetY : Int
    }


onMouseMove : (MouseMoveEvent -> msg) -> Attribute msg
onMouseMove tagger =
    on "mousemove" (Json.map tagger mouseMoveDecoder)


onMouseDown : (MouseMoveEvent -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Json.map tagger mouseMoveDecoder)


onMouseMoveAlwaysStopPropagation : (MouseMoveEvent -> msg) -> Attribute msg
onMouseMoveAlwaysStopPropagation tagger =
    stopPropagationOn "mousemove" (Json.map alwaysStop (Json.map tagger mouseMoveDecoder))


onMouseUp : msg -> Attribute msg
onMouseUp tagger =
    on "mouseup" (Json.succeed tagger)



mouseMoveDecoder : Json.Decoder MouseMoveEvent
mouseMoveDecoder =
    Json.map7 MouseMoveEvent
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)
        (Json.field "layerX" Json.int)
        (Json.field "layerY" Json.int)
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)
        (Json.field "buttons" Json.int)



onClickAlwaysStopPropagation : msg -> Attribute msg
onClickAlwaysStopPropagation msg =
    stopPropagationOn "click" (Json.map alwaysStop (Json.succeed msg))


alwaysStop x =
    ( x, True )



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
