module DragState exposing (..)

import ExtraEvents exposing (MouseDownEvent, MouseMoveEvent, emptyElement)
import Html exposing (Html, div)
import Html.Attributes exposing (class, coords, style)
import Ports
import Tree exposing (TreeItem, hasId)


rowHeight =
    35


type DragState
    = NoDrag
    | PressedNotYetMoved MouseMoveEvent String Float
    | DraggingSomething MouseMoveEvent String


type DragMsg
    = MouseUp
    | MouseMove MouseMoveEvent
    | MouseDown String MouseDownEvent


initialState : DragState
initialState =
    NoDrag


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
                        DraggingSomething newMousePosition id

                    else
                        PressedNotYetMoved newMousePosition id newDistance

                DraggingSomething _ id ->
                    DraggingSomething newMousePosition id

        MouseUp ->
            NoDrag


handleItemDrop model =
    let
        coordsMaybe =
            getDraggingCoords model.dragState

        nodeIdBeingDragged =
            getNodeBeingDragged model.dragState |> Maybe.withDefault ""
    in
    case ( coordsMaybe, Tree.find (hasId nodeIdBeingDragged) model.nodes ) of
        ( Just coords, Just nodeBeingDragged ) ->
            let
                dropIndicator =
                    getDropIndicatorPosition model.nodes coords

                nodeUpdater =
                    case dropIndicator.dropPlacement of
                        DropAfterNode ->
                            Tree.insertAfterNode

                        DropBeforeNode ->
                            Tree.insertBeforeNode

                        DropInsideNode ->
                            Tree.insertAsFirstChild

                nodes =
                    model.nodes
                        |> Tree.removeNode nodeIdBeingDragged
                        |> nodeUpdater nodeBeingDragged dropIndicator.nodeUnder
            in
            ( { model
                | dragState = update model.dragState MouseUp
                , nodes = nodes
              }
            , Ports.sendEndDrag
            )

        _ ->
            ( { model | dragState = update model.dragState MouseUp }, Ports.sendEndDrag )


type alias DropTargetInfo =
    { x : Int, y : Int, nodeUnder : String, dropPlacement : DropPlacement }


type DropPlacement
    = DropAfterNode
    | DropBeforeNode
    | DropInsideNode


getDropIndicatorPosition : List TreeItem -> MouseMoveEvent -> DropTargetInfo
getDropIndicatorPosition nodes mouseMoveEvent =
    let
        yPosition =
            mouseMoveEvent.layerY

        nodeUnder =
            Tree.findNodeByYCoordinates (yPosition // rowHeight) nodes

        nodeUnderLevel =
            nodeUnder |> Maybe.map .level |> Maybe.withDefault 0

        mouseLevel =
            mouseMoveEvent.layerX // 31

        isShownUnder =
            nodeUnderLevel < mouseLevel

        dropIndicatorLeftPosition =
            if isShownUnder && isOnSecondHalf then
                (nodeUnderLevel + 1) * 31

            else
                nodeUnderLevel * 31

        dropPlacement =
            if isShownUnder && isOnSecondHalf then
                DropInsideNode

            else if isOnSecondHalf then
                DropAfterNode

            else
                DropBeforeNode

        isOnSecondHalf =
            remainderBy rowHeight yPosition > (rowHeight // 2)
    in
    { x = dropIndicatorLeftPosition
    , nodeUnder = nodeUnder |> Maybe.map .id |> Maybe.withDefault ""
    , dropPlacement = dropPlacement
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


viewDragIndicator : DragState -> List TreeItem -> Html msg
viewDragIndicator dragState nodes =
    let
        viewDropIndicator dropPosition =
            div
                [ class "drop-indicator"
                , style "top" (String.fromInt dropPosition.y ++ "px")
                , style "left" (String.fromInt dropPosition.x ++ "px")
                ]
                []
    in
    getDraggingCoords dragState
        |> Maybe.map (viewDropIndicator << getDropIndicatorPosition nodes)
        |> Maybe.withDefault emptyElement


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


shouldListenToDragEvents : DragState -> Bool
shouldListenToDragEvents dragState =
    case dragState of
        NoDrag ->
            False

        _ ->
            True


getNodeBeingDragged dragState =
    case dragState of
        DraggingSomething _ id ->
            Just id

        _ ->
            Nothing


getDraggingCoords dragState =
    case dragState of
        DraggingSomething event _ ->
            Just event

        _ ->
            Nothing


isDraggingNode dragState nodeId =
    case dragState of
        DraggingSomething _ id ->
            nodeId == id

        _ ->
            False


isDragging dragState =
    case dragState of
        DraggingSomething _ _ ->
            True

        _ ->
            False
