module DragState exposing (..)

import ExtraEvents exposing (MouseDownEvent, MouseMoveEvent)


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


getNodeBeingDraggedOver dragState =
    case dragState of
        DraggingSomething _ _ idMaybe ->
            idMaybe |> Maybe.map .itemId

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


isDragging dragState =
    case dragState of
        DraggingSomething _ _ _ ->
            True

        _ ->
            False


isDragginOnBottom dragState =
    case dragState of
        DraggingSomething _ _ itemUnderInfo ->
            case itemUnderInfo of
                Just itemUnder ->
                    itemUnder.verticalOffset > 15

                _ ->
                    False

        _ ->
            False
