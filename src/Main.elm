module Main exposing (..)

import Browser
import Debug exposing (log)
import DragState exposing (DragState(..), rowHeight)
import ExtraEvents exposing (MouseMoveEvent, attributeIf, classIf, elementIf, onMouseMove, onMouseUp)
import Html exposing (Attribute, Html, div, img, span, text)
import Html.Attributes exposing (class, draggable, src, style)
import Html.Events exposing (onClick)
import MaybeExtra
import Tree exposing (NodePayload(..), TreeItem, getChildrenForNode, hasId, initialNodes)


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
    | DropItem
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus itemId ->
            ( { model | focus = Node itemId }, Cmd.none )

        RemoveFocus ->
            ( { model | focus = Root }, Cmd.none )

        DropItem ->
            let
                a =
                    DragState.getDraggingCoords model.dragState

                getCoord b =
                    Tree.findNodeByYCoordinates (b.layerY // rowHeight) model.nodes

                foo =
                    log "drop" (a |> Maybe.map getCoord |> MaybeExtra.join |> Maybe.map .title)
            in
            ( { model
                | dragState = DragState.update model.dragState DragState.MouseUp
                , nodes = DragState.handlePlacement model.nodes model.dragState
              }
            , Cmd.none
            )

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
    let
        isListeningToEvents =
            DragState.shouldListenToDragEvents model.dragState
    in
    div
        [ attributeIf isListeningToEvents (onMouseMove (\n -> DndAction (DragState.MouseMove n)))
        , attributeIf isListeningToEvents (onMouseUp DropItem)
        , class "page"
        , classIf (DragState.isDragging model.dragState) "page-during-drag"
        ]
        [ viewHeader model
        , viewSample model
        , DragState.viewDragIndicator model.dragState model.nodes
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
                            , style "top" (String.fromInt mousePosition.layerY ++ "px")
                            , style "left" (String.fromInt mousePosition.layerX ++ "px")
                            ]
                            [ div [ class "bullet-outer" ] [ div [ class "bullet" ] [] ], text node.title ]
                        ]

                Nothing ->
                    div [] []

        _ ->
            div [] []


viewSample : Model -> Html Msg
viewSample model =
    let
        focusPointMaybe =
            case model.focus of
                Node id ->
                    Tree.find (hasId id) model.nodes

                Root ->
                    Nothing
    in
    case focusPointMaybe of
        Just node ->
            div
                []
                [ div [ class "focused-element" ] [ text node.title ]
                , div [] (List.map (viewNode model.dragState) (getChildrenForNode node))
                ]

        Nothing ->
            div [] (List.map (viewNode model.dragState) model.nodes)


viewNode : DragState -> TreeItem -> Html Msg
viewNode dragState node =
    div [ class "row" ]
        [ div [ class "row-title" ]
            [ viewNodeImage
                [ attributeIf (not (DragState.isDraggingNode dragState node.id)) (onClick (SetFocus node.id))
                , ExtraEvents.onMouseDown (\n -> DndAction (DragState.MouseDown node.id n))
                ]
                node.payload
            , div [ onClick (ToggleVisibility node.id) ] [ text node.title ]
            ]
        , elementIf node.isVisible
            (div [ class "children-area" ]
                (List.map (viewNode dragState) (getChildrenForNode node))
            )
        ]


viewNodeImage attributes payload =
    case payload of
        Playlist ->
            div
                (List.append attributes
                    [ class "bullet-outer"
                    ]
                )
                []

        Video id ->
            img
                (List.append attributes
                    [ class "bullet-outer video-image"
                    , src ("https://i.ytimg.com/vi/" ++ id ++ "/default.jpg")
                    , draggable "false"
                    ]
                )
                []
