module Main exposing (..)

import Browser
import Debug exposing (log)
import DragState exposing (DragState(..), getNodeBeingDragged, getNodeBeingDraggedOver)
import ExtraEvents exposing (attributeIf, classIf, elementIf, onMouseMove, onMouseUp)
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Tree exposing (TreeItem, hasId, initialNodes)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { nodes : List TreeItem
    , focus : Focus
    , dragState : DragState.DragState
    }


type Focus
    = Root
    | Node String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = initialNodes
      , focus = Root
      , dragState = DragState.initialState
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ToggleVisibility String
    | SetFocus String
    | RemoveFocus
    | DndAction DragState.DragMsg
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus itemId ->
            ( { model | focus = Node itemId }, Cmd.none )

        RemoveFocus ->
            ( { model | focus = Root }, Cmd.none )

        DndAction subMsg ->
            ( { model | dragState = DragState.update model.dragState subMsg }, Cmd.none )

        ToggleVisibility id ->
            let
                toggleVisibility item =
                    if item.id == id then
                        { item | isVisible = not item.isVisible }

                    else
                        item
            in
            ( { model | nodes = Tree.mapAllNodes toggleVisibility model.nodes }, Cmd.none )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ attributeIf (DragState.shouldListenToDragEvents model.dragState) (onMouseMove (\n -> DndAction (DragState.MouseMove n)))
        , attributeIf (DragState.shouldListenToDragEvents model.dragState) (onMouseUp (DndAction DragState.MouseUp))
        , class "page"
        , classIf (DragState.isDragging model.dragState) "page-during-drag"
        ]
        [ viewHeader model
        , viewPage model
        , viewNodeBeingDragged model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "header" ]
        (case model.focus of
            Node nodeId ->
                let
                    viewHome =
                        span [ class "clickable-text breadcrumb-part", onClick RemoveFocus ] [ text "Home" ]

                    parents =
                        Tree.getParents model.nodes nodeId

                    selectedNode =
                        Tree.find (\n -> n.id == nodeId) model.nodes

                    viewClickablePart : TreeItem -> Html Msg
                    viewClickablePart node =
                        span [ class "clickable-text breadcrumb-part", onClick (SetFocus node.id) ] [ text node.title ]

                    intermediateParts =
                        if List.isEmpty parents then
                            []

                        else
                            List.map viewClickablePart parents ++ [ viewSplitter ]

                    viewNonClickablePart : Maybe TreeItem -> Html Msg
                    viewNonClickablePart val =
                        Maybe.map (\n -> span [ class "breadcrumb-part" ] [ text n.title ]) val |> Maybe.withDefault (span [] [])

                    viewSplitter =
                        span [ class "splitter" ] [ text " > " ]
                in
                [ [ viewHome, viewSplitter ], intermediateParts, [ viewNonClickablePart selectedNode ] ] |> List.concat

            Root ->
                []
        )


viewNodeBeingDragged : Model -> Html Msg
viewNodeBeingDragged model =
    case model.dragState of
        DraggingSomething mousePosition itemId _ ->
            case Tree.find (hasId itemId) model.nodes of
                Just node ->
                    div [ class "box-container" ]
                        [ div
                            [ class "box"
                            , style "top" (String.fromInt mousePosition.pageY ++ "px")
                            , style "left" (String.fromInt mousePosition.pageX ++ "px")
                            ]
                            [ div [ class "bullet-outer" ] [ div [ class "bullet" ] [] ], text node.title ]
                        ]

                Nothing ->
                    div [] []

        _ ->
            div [] []


viewPage : Model -> Html Msg
viewPage model =
    let
        flatNodes =
            Tree.getNodesFlattenedWithLevels model.nodes

        nodeFocused =
            List.filter (hasId (nodeIdFocused model.focus)) flatNodes
                |> List.head
                |> Maybe.map .level
                |> Maybe.withDefault 0
                |> (*) -20
                |> log "left"

        indexOfNode =
            List.indexedMap Tuple.pair flatNodes
                |> List.filter (\t -> (Tuple.second t).id == nodeIdFocused model.focus)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
                |> (*) -20
                |> log "top"
    in
    div
        [ class "nodes-container"
        , style "margin-top" (String.fromInt indexOfNode ++ "px")
        , style "margin-left" (String.fromInt nodeFocused ++ "px")
        ]
        (List.indexedMap (viewNode model) flatNodes)


viewNode : Model -> Int -> TreeItem -> Html Msg
viewNode model sequentialPosition node =
    let
        nodeBeingDraggedId =
            getNodeBeingDragged model.dragState

        nodeBeingDraggedOverId =
            getNodeBeingDraggedOver model.dragState

        shouldListenToDragEvents =
            DragState.shouldListenToDragEvents model.dragState
    in
    div
        [ class "row"
        , style "top" (String.fromInt (sequentialPosition * 20) ++ "px")
        , style "left" (String.fromInt (node.level * 20) ++ "px")
        , classIf (maybeHasValue nodeBeingDraggedId node.id) "row-being-dragged"
        , attributeIf shouldListenToDragEvents (ExtraEvents.onMouseMove (\n -> DndAction (DragState.MouseOver n node.id)))
        ]
        [ div
            [ class "bullet-outer"
            , onClick (SetFocus node.id)
            , ExtraEvents.onMouseDown (\n -> DndAction (DragState.MouseDown node.id n))
            ]
            [ div [ class "bullet" ] [] ]
        , span [ class "clickable-text", onClick (ToggleVisibility node.id) ]
            [ text node.title ]
        , elementIf (maybeHasValue nodeBeingDraggedOverId node.id) (viewPlacementText model.dragState)
        ]


viewPlacementText dragState =
    let
        message =
            if DragState.isDragginOnBottom dragState then
                "place after"

            else
                "place before"
    in
    span [ class "placement-text" ] [ text message ]


nodeIdFocused focus =
    case focus of
        Node id ->
            id

        Root ->
            ""

maybeHasValue maybe value =
    case maybe of
        Just val ->
            val == value

        Nothing ->
            False
