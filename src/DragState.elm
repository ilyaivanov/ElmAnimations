module DragState exposing (..)

import ExtraEvents exposing (MouseDownEvent, MouseMoveEvent, emptyElement)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Tree exposing (TreeItem, findNodeByYCoordinates)


rowHeight =
    35


type DragState
    = NoDrag
    | PressedNotYetMoved MouseMoveEvent String Float
    | DraggingSomething MouseMoveEvent String (Maybe DragOverInfo)


type alias DragOverInfo =
    { itemId : String
    , verticalOffset : Int
    , horizontalOffset : Int
    }


type DragMsg
    = MouseUp
    | MouseMove MouseMoveEvent
    | MouseDown String MouseDownEvent
    | MouseOver MouseMoveEvent String


shouldListenToDragEvents : DragState -> Bool
shouldListenToDragEvents dragState =
    case dragState of
        NoDrag ->
            False

        _ ->
            True


initialState : DragState
initialState =
    NoDrag


getNodeBeingDragged dragState =
    case dragState of
        DraggingSomething _ id _ ->
            Just id

        _ ->
            Nothing


getDraggingCoords dragState =
    case dragState of
        DraggingSomething event _ _ ->
            Just event

        _ ->
            Nothing


update : DragState -> DragMsg -> DragState
update state msg =
    case msg of
        MouseDown nodeId event ->
            PressedNotYetMoved event.mousePosition nodeId 0

        MouseMove newMousePosition ->
            case state of
                NoDrag ->
                    state

                PressedNotYetMoved previousPosition id distance ->
                    let
                        newDistance =
                            distance + getDistance previousPosition newMousePosition
                    in
                    --makes much a better UX - we need to drag at least 3 pixels in order to count as drag
                    --otherwise this is considered a click (play if clicking on a card)
                    if newDistance > 5 then
                        DraggingSomething newMousePosition id Nothing

                    else
                        PressedNotYetMoved newMousePosition id newDistance

                DraggingSomething _ id info ->
                    DraggingSomething newMousePosition id info

        MouseOver _ itemId ->
            case state of
                DraggingSomething event id _ ->
                    DraggingSomething event id (Just { itemId = itemId, horizontalOffset = event.layerX, verticalOffset = event.layerY })

                _ ->
                    state

        MouseUp ->
            NoDrag



isDraggingOverSecondHalf dragState =
    case dragState of
        DraggingSomething newMousePosition _ _ ->
            remainderBy rowHeight newMousePosition.layerY > (rowHeight // 2)

        _ ->
            False


--noinspection ElmUnusedSymbol
viewDragIndicator : DragState -> List TreeItem -> Html msg
viewDragIndicator dragState nodes =
    case dragState of
        DraggingSomething newMousePosition nodeBeingDragged _ ->
            let
                yPosition =
                    newMousePosition.layerY

                --This will be used to restrict drop-indicator position
                nodeUnder =
                    findNodeByYCoordinates (yPosition // rowHeight) nodes

                isOnSecondHalf =
                    isDraggingOverSecondHalf dragState

                nodesPosition =
                    { x = newMousePosition.layerX // rowHeight * rowHeight
                    , y =
                        yPosition
                            // rowHeight
                            * rowHeight
                            + (if isOnSecondHalf then
                                rowHeight

                               else
                                0
                              )
                    }
            in
            div
                [ class "drop-indicator"
                , style "top" (String.fromInt nodesPosition.y ++ "px")
                ]
                []

        _ ->
            emptyElement


getDistance : MouseMoveEvent -> MouseMoveEvent -> Float
getDistance point1 point2 =
    sqrt
        (toFloat
            ((point1.pageX - point2.pageX)
                ^ 2
                + (point1.pageY - point2.pageY)
                ^ 2
            )
        )


isDraggingNode dragState nodeId =
    case dragState of
        DraggingSomething _ id _ ->
            nodeId == id

        _ ->
            False


isDragging dragState =
    case dragState of
        DraggingSomething _ _ _ ->
            True

        _ ->
            False
