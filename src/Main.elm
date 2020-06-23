module Main exposing (..)

import Browser
import DragState exposing (DragState(..))
import ExtraEvents exposing (MouseMoveEvent, attributeIf, classIf, elementIf, onMouseMove, onMouseUp)
import Html exposing (Attribute, Html, button, div, img, input, span, text)
import Html.Attributes exposing ( class, draggable, id, src, style, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Json
import Ports
import Tree exposing (NodePayload(..), TreeItem, getChildrenForNode, hasId, initialNodes)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { nodes : List TreeItem
    , focus : Focus
    , dragState : DragState.DragState
    , nodeBeingEdited : Maybe EditedNodeState
    }


type Focus
    = Root
    | Node String


type alias EditedNodeState =
    { id : String, text : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = initialNodes
      , focus = Root
      , dragState = DragState.initialState
      , nodeBeingEdited = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onWindowKeyUp mapLoadedBoards
        , Ports.gotNewId AddNewNode
        ]



mapLoadedBoards : Json.Value -> Msg
mapLoadedBoards modelJson =
    case Json.decodeValue decodeBoard modelJson of
        Ok model ->
            OnKeyPressed model

        Err _ ->
            None


decodeBoard : Json.Decoder KeyPressedEvent
decodeBoard =
    Json.map KeyPressedEvent
        (Json.field "key" Json.string)


type Msg
    = ToggleVisibility String
    | SetFocus String
    | RemoveFocus
    | RemoveNode String
    | AddNewNodeClicked
    | AddNewNode String
    | DndAction DragState.DragMsg
    | DropItem
    | EditNode String
    | SetEditText String
    | CompleteEdit
    | RevertEdit
    | OnKeyPressed KeyPressedEvent
    | None


type alias KeyPressedEvent =
    { key : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus itemId ->
            ( { model | focus = Node itemId }, Cmd.none )

        RemoveFocus ->
            ( { model | focus = Root }, Cmd.none )

        RemoveNode nodeId ->
            ( { model | nodes = Tree.removeNode nodeId model.nodes }, Cmd.none )

        DropItem ->
            DragState.handleItemDrop model

        EditNode nodeId ->
            let
                text =
                    Tree.find (hasId nodeId) model.nodes |> Maybe.map .title |> Maybe.withDefault ""
            in
            ( { model | nodeBeingEdited = Just { id = nodeId, text = text } }, Ports.onEditStart nodeId )

        SetEditText newText ->
            let
                updateText node =
                    { node | text = newText }

                newEditState =
                    model.nodeBeingEdited |> Maybe.map updateText
            in
            ( { model | nodeBeingEdited = newEditState }, Cmd.none )

        CompleteEdit ->
            let
                newText =
                    model.nodeBeingEdited |> Maybe.map .text |> Maybe.withDefault ""

                newId =
                    model.nodeBeingEdited |> Maybe.map .id |> Maybe.withDefault ""

                setText item =
                    if item.id == newId then
                        { item | title = newText }

                    else
                        item
            in
            ( { model | nodeBeingEdited = Nothing, nodes = Tree.mapAllNodes setText model.nodes }, Cmd.none )

        RevertEdit ->
            ( { model | nodeBeingEdited = Nothing }, Cmd.none )

        DndAction subMsg ->
            let
                newDragState =
                    DragState.update model.dragState subMsg

                haStartedToDrag =
                    DragState.isDragging newDragState && not (DragState.isDragging model.dragState)

                cmd =
                    if haStartedToDrag then
                        Ports.sendStartDrag

                    else
                        Cmd.none
            in
            ( { model | dragState = newDragState }, cmd )

        ToggleVisibility id ->
            let
                toggleVisibility item =
                    if item.id == id then
                        { item | isVisible = not item.isVisible }

                    else
                        item
            in
            ( { model | nodes = Tree.mapAllNodes toggleVisibility model.nodes }, Cmd.none )

        OnKeyPressed event ->
            case event.key of
                "Escape" ->
                    update RevertEdit model

                "Enter" ->
                    update CompleteEdit model

                _ ->
                    ( model, Cmd.none )

        AddNewNodeClicked ->
            ( model, Ports.requestNewId )

        AddNewNode newId ->
            let
                nodes =
                    List.append model.nodes [ Tree.createNewNode newId ]
            in
            ( { model | nodes = nodes }, Cmd.none )

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
        , button [ onClick AddNewNodeClicked ] [ text "Add" ]
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
        DraggingSomething mousePosition itemId ->
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
                , div [] (List.map (viewNode model.dragState model.nodeBeingEdited) (getChildrenForNode node))
                ]

        Nothing ->
            div [] (List.map (viewNode model.dragState model.nodeBeingEdited) model.nodes)


viewNode : DragState -> Maybe EditedNodeState -> TreeItem -> Html Msg
viewNode dragState nodeEditedState node =
    div [ class "row" ]
        [ div [ class "row-title" ]
            [ viewNodeImage
                [ attributeIf (not (DragState.isDraggingNode dragState node.id)) (onClick (SetFocus node.id))
                , ExtraEvents.onMouseDown (\n -> DndAction (DragState.MouseDown node.id n))
                ]
                node.payload
            , viewText nodeEditedState node
            , div [ class "row-icons" ]
                [ span [ class "row-icon", onClick (EditNode node.id) ] [ text "E" ]
                , span [ class "row-icon", onClick (RemoveNode node.id) ] [ text "X" ]
                ]
            ]
        , elementIf node.isVisible
            (div [ class "children-area" ]
                (List.map (viewNode dragState nodeEditedState) (getChildrenForNode node))
            )
        ]


viewText nodeEditedState node =
    let
        nodeId =
            nodeEditedState |> Maybe.map .id |> Maybe.withDefault ""

        nodeText =
            nodeEditedState |> Maybe.map .text |> Maybe.withDefault ""
    in
    if nodeId == node.id then
        input
            [ id ("input" ++ nodeId)
            , class "row-title-input"
            , onInput SetEditText
            , onBlur CompleteEdit
            , value nodeText
            ]
            []

    else
        div [ class "row-title-text", onClick (ToggleVisibility node.id) ] [ text node.title ]


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
