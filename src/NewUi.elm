module NewUi exposing (main)

import Assets
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import ExtraEvents exposing (MouseMoveEvent, attributeIf, classIf, emptyElement, onClickAlwaysStopPropagation, onMouseDown, onMouseMove, onMouseMoveAlwaysStopPropagation, onMouseUp)
import Html exposing (Attribute, Html, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src, style)
import Html.Events exposing (onClick)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { tree : HashTree
    , focusedNodeId : String
    , dragState : DragState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree = sampleData
      , focusedNodeId = homeId
      , dragState = NoDrag
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = None
    | Focus String
    | Toggle String
    | MouseDownOnCircle String MouseMoveEvent
    | MouseMove MouseMoveEvent
    | MouseMoveOverNode String MouseMoveEvent
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus id ->
            ( { model | focusedNodeId = id }, Cmd.none )

        MouseDownOnCircle id event ->
            ( { model | dragState = updateOnMouseDown event id }, Cmd.none )

        MouseMoveOverNode id event ->
            ( { model | dragState = updateOnMouseMove model.dragState event (Just id) }, Cmd.none )

        MouseMove event ->
            ( { model | dragState = updateOnMouseMove model.dragState event Nothing }, Cmd.none )

        MouseUp ->
            ( updateOnDrop model, Cmd.none )

        Toggle id ->
            ( { model | tree = toggleVisibility model.tree id }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "page"
        , classIf (isDraggingSomething model.dragState) "page-during-drag"
        , attributeIf (shouldListenToDragEvents model.dragState) (onMouseMove MouseMove)
        , attributeIf (shouldListenToDragEvents model.dragState) (onMouseUp MouseUp)
        ]
        [ viewSidebar model
        , viewTree model
        , viewDragItem model

        --, viewSearch
        ]


viewDragItem : Model -> Html Msg
viewDragItem model =
    case getDraggingCoords model.dragState of
        Just coords ->
            div
                [ class "drag-bolt"
                , style "top" (String.fromInt coords.pageY ++ "px")
                , style "left" (String.fromInt coords.pageX ++ "px")
                ]
                []

        Nothing ->
            emptyElement


viewSidebar model =
    div [ class "sidebar" ]
        [ viewChildren model (getHomeItems model.tree)
        ]


viewChildren model childs =
    div [ class "sidebar-items-children" ]
        (List.map (viewSidebarItem model)
            childs
        )


viewSidebarItem : Model -> TreeItem -> Html Msg
viewSidebarItem model item =
    div []
        [ div
            [ class "sidebar-item"
            , classIf (model.focusedNodeId == item.id) "focused"
            , onClick (Focus item.id)
            ]
            [ div
                [ onClickAlwaysStopPropagation (Toggle item.id)
                , class "sidebar-item-chevron"
                , classIf item.isVisible "open"
                ]
                [ img [ src Assets.chevron ] [] ]
            , div [] [ text item.title ]
            ]
        , if item.isVisible then
            getChildren model.tree item.id |> viewChildren model

          else
            emptyElement
        ]


viewSearch model =
    div [ class "search" ]
        [ input [ placeholder "Search for videos, channels", class "search-input" ] []
        , viewTree model
        ]


viewTree model =
    div [ class "tree" ]
        [ viewHeader model, viewTreeBody model ]


viewHeader model =
    if model.focusedNodeId == homeId then
        emptyElement

    else
        let
            parents =
                getParents model.tree model.focusedNodeId

            focusedNode =
                findById model.focusedNodeId model.tree |> Maybe.map .title |> Maybe.withDefault ""
        in
        div [ class "header" ]
            (parents
                |> List.map viewHeaderPart
                |> List.concat
                |> flip List.append [ viewFocusedHeaderPart focusedNode ]
            )


viewHeaderPart n =
    [ div [ class "header-item", onClick (Focus n.id) ] [ text n.title ]
    , div [] [ text ">" ]
    ]


viewFocusedHeaderPart title =
    div [ class "header-item" ] [ text title ]


viewTreeBody model =
    viewHomeChildren model (getChildren model.tree model.focusedNodeId)


viewHomeChildren model nodes =
    div [ class "children" ]
        (nodes |> List.map (viewHomeNode model))


viewHomeNode model n =
    let
        title =
            case n.payload of
                Playlist ->
                    node model n (playlistIcon model n)

                Video info ->
                    node model n (videoIcon info.videoId)
    in
    div []
        [ title
        , if n.isVisible then
            getChildren model.tree n.id |> viewHomeChildren model

          else
            emptyElement
        ]


node model n iconElement =
    let
        dropIndicator =
            case getItemUnder model.dragState of
                Just ( event, id ) ->
                    if n.id == id then
                        case getDragPlacement event of
                            AfterNode ->
                                dropPlaceholderAfter

                            BeforeNode ->
                                dropPlaceholderBefore

                            InsideNode ->
                                dropPlaceholderInside

                    else
                        emptyElement

                Nothing ->
                    emptyElement
    in
    div
        [ class "node-title"
        , attributeIf (shouldListenToDragEvents model.dragState) (onMouseMoveAlwaysStopPropagation (MouseMoveOverNode n.id))
        ]
        [ div [ class "branch" ] []
        , div [ class "branch-bubble" ] []
        , iconElement
        , div [ class "node-title-text" ] [ text n.title ]
        , div [ class "row-icon" ] [ text "X" ]
        , dropIndicator
        ]


dropPlaceholderBefore =
    div [ class "drop-placeholder-before" ]
        [ div [ class "small-circle" ] []
        ]


dropPlaceholderAfter =
    div [ class "drop-placeholder-after" ]
        [ div [ class "small-circle" ] []
        ]


dropPlaceholderInside =
    div [ class "drop-placeholder-inside" ]
        [ div [ class "small-circle" ] []
        ]


videoIcon videoId =
    img [ src ("https://i.ytimg.com/vi/" ++ videoId ++ "/mqdefault.jpg"), class "image" ] []


playlistIcon model n =
    div
        [ class "circle"
        , classIf (getItemBeingDragged model.dragState |> hasValue n.id) "being-dragged"
        , onClick (Focus n.id)
        , onMouseDown (MouseDownOnCircle n.id)
        ]
        []



--TREE STRUCTURE


type alias TreeItem =
    { id : String
    , title : String
    , isVisible : Bool
    , children : List String
    , payload : NodeType
    }


type NodeType
    = Playlist
    | Video VideoInfo


type alias VideoInfo =
    { videoId : String }


type alias HashTree =
    Dict String TreeItem


sampleData : HashTree
sampleData =
    Dict.fromList
        [ channel homeId homeId [ "1", "2" ]
        , channel "1" "Ambient" [ "1.1", "1.2", "1.3" ]
        , leafChannel "1.1" "Ambient Child 1"
        , leafChannel "1.2" "Ambient Child 2"
        , channel "1.3" "Ambient Child 3" [ "1.3.1", "1.3.2", "1.3.3" ]
        , leafVideo "1.3.1" "Ambient Child 3 Video 1" "gmQJVl51yCc"
        , leafVideo "1.3.2" "Ambient Child 3 Video 2" "5o_uF1L5l6o"
        , leafVideo "1.3.3" "Ambient Child 3 Video 3" "tDolNU89SXI"
        , leafChannel "2" "Deep House"
        ]


leafChannel id title =
    channel id title []


leafVideo id title videoId =
    ( id, TreeItem id title True [] (Video { videoId = videoId }) )


channel id title children =
    ( id, TreeItem id title True children Playlist )


getHomeItems : HashTree -> List TreeItem
getHomeItems hasTree =
    getChildren hasTree homeId


getChildren : HashTree -> String -> List TreeItem
getChildren hashTree nodeId =
    let
        childrenIds =
            hashTree
                |> Dict.get nodeId
                |> Maybe.map .children
                |> Maybe.withDefault []
    in
    childrenIds
        |> List.map (flip Dict.get hashTree)
        |> filterOutNothing


getParents : HashTree -> String -> List TreeItem
getParents tree id =
    getParentsImp tree id []


getParentsImp tree id parents =
    case findBy (hasChild id) tree of
        Just parentNode ->
            getParentsImp tree parentNode.id (parentNode :: parents)

        Nothing ->
            parents


toggleVisibility : HashTree -> String -> HashTree
toggleVisibility hash id =
    mapItem id (\item -> { item | isVisible = not item.isVisible }) hash


removeFromParent id tree =
    mapParent id (\parent -> { parent | children = parent.children |> List.filter ((/=) id) }) tree


insertItemBefore itemBeforeId itemId tree =
    let
        mapChild child =
            if child == itemBeforeId then
                [ itemId, child ]

            else
                [ child ]
    in
    mapParent itemBeforeId (\item -> { item | children = item.children |> List.map mapChild |> List.concat }) tree


insertItemAfter itemBeforeId itemId tree =
    let
        mapChild child =
            if child == itemBeforeId then
                [ child, itemId ]

            else
                [ child ]
    in
    mapParent itemBeforeId (\item -> { item | children = item.children |> List.map mapChild |> List.concat }) tree


insertItemInside parentId newItemId tree =
    mapItem parentId (\item -> { item | children = newItemId :: item.children }) tree


hasChild id n =
    List.member id n.children


findBy : (TreeItem -> Bool) -> HashTree -> Maybe TreeItem
findBy predicate tree =
    Dict.values tree |> List.filter predicate |> List.head


findById id tree =
    Dict.get id tree


mapItem id mapper tree =
    case Dict.get id tree of
        Just item ->
            Dict.insert id (mapper item) tree

        Nothing ->
            tree


mapParent id mapper tree =
    case findBy (hasChild id) tree of
        Just parent ->
            tree |> Dict.insert parent.id (mapper parent)

        Nothing ->
            tree


homeId =
    "Home"



--DRAG STATE


type DragState
    = NoDrag
    | PressedNotYetMoved MouseMoveEvent String Float
    | DraggingSomething MouseMoveEvent String (Maybe String)


shouldListenToDragEvents : DragState -> Bool
shouldListenToDragEvents dragState =
    case dragState of
        NoDrag ->
            False

        _ ->
            True


isDraggingSomething dragState =
    getDraggingCoords dragState |> hasSomething


getDraggingCoords dragState =
    case dragState of
        DraggingSomething event _ _ ->
            Just event

        _ ->
            Nothing


getItemUnder : DragState -> Maybe ( MouseMoveEvent, String )
getItemUnder dragState =
    case dragState of
        DraggingSomething event _ idMaybe ->
            idMaybe |> Maybe.map (Tuple.pair event)

        _ ->
            Nothing


getItemBeingDragged : DragState -> Maybe String
getItemBeingDragged dragState =
    case dragState of
        DraggingSomething _ id _ ->
            Just id

        _ ->
            Nothing


updateOnMouseDown event nodeId =
    PressedNotYetMoved event nodeId 0


updateOnMouseMove state newMousePosition overItem =
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
                DraggingSomething newMousePosition id overItem

            else
                PressedNotYetMoved newMousePosition id newDistance

        DraggingSomething _ id _ ->
            DraggingSomething newMousePosition id overItem


updateOnDrop model =
    let
        itemIdOverM =
            getItemBeingDragged model.dragState

        itemIdUnderM =
            getItemUnder model.dragState
    in
    case ( itemIdUnderM, itemIdOverM ) of
        ( Just ( event, itemIdUnder ), Just itemIdOver ) ->
            let
                modifier =
                    case getDragPlacement event of
                        AfterNode ->
                            insertItemAfter

                        BeforeNode ->
                            insertItemBefore

                        InsideNode ->
                            insertItemInside

                foo =
                    model.tree
                        |> removeFromParent itemIdOver
                        |> modifier itemIdUnder itemIdOver
            in
            { model | dragState = NoDrag, tree = foo }

        _ ->
            { model | dragState = NoDrag }


type DragPlacement
    = AfterNode
    | BeforeNode
    | InsideNode



--TODO: this doesn't account for different height in playlist/video nodes
-- works only for playlist nodes right now
-- also align node to the center (current margin only from the top
-- also offsetY behaves very strange - it sends 0-10 in padded area, then in container starts from the zero again


getDragPlacement event =
    if event.offsetY > 25 then
        if event.offsetX > 1030 then
            InsideNode

        else
            AfterNode

    else
        BeforeNode


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



--****EXTRA****
--FUNCTIONS


flip func a b =
    func b a



-- LISTS


filterOutNothing : List (Maybe a) -> List a
filterOutNothing maybes =
    List.filterMap identity maybes



-- Maybe


hasSomething maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


hasValue value maybe =
    case maybe of
        Just maybeValue ->
            maybeValue == value

        Nothing ->
            False
