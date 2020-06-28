module NewUi exposing (main)

import Assets
import Browser
import Dict exposing (Dict)
import ExtraEvents exposing (classIf, emptyElement, onClickAlwaysStopPropagation)
import Html exposing (Attribute, Html, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src)
import Html.Events exposing (onClick)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { tree : HashTree
    , focusedNodeId : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree = sampleData
      , focusedNodeId = homeId
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus id ->
            ( { model | focusedNodeId = id }, Cmd.none )

        Toggle id ->
            ( { model | tree = toggleVisibility model.tree id }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ viewSidebar model
        , viewTree model

        --, viewSearch
        ]


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
                    node n.title (playlistIcon n)

                Video info ->
                    node n.title (videoIcon info.videoId)
    in
    div []
        [ title
        , if n.isVisible then
            getChildren model.tree n.id |> viewHomeChildren model

          else
            emptyElement
        ]


node title iconElement =
    div [ class "node-title" ]
        [ div [ class "branch" ] []
        , div [ class "branch-bubble" ] []
        , iconElement
        , div [ class "node-title-text" ] [ text title ]
        , div [ class "row-icon" ] [ text "X" ]
        ]


videoIcon videoId =
    img [ src ("https://i.ytimg.com/vi/" ++ videoId ++ "/mqdefault.jpg"), class "image" ] []


playlistIcon n =
    div [ class "circle", onClick (Focus n.id) ] []



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


hasChild id n =
    List.member id n.children


findBy : (TreeItem -> Bool) -> HashTree -> Maybe TreeItem
findBy predicate tree =
    Dict.values tree |> List.filter predicate |> List.head


findById id tree =
    Dict.get id tree


toggleVisibility : HashTree -> String -> HashTree
toggleVisibility hash id =
    case Dict.get id hash of
        Just item ->
            Dict.insert id { item | isVisible = not item.isVisible } hash

        Nothing ->
            hash


homeId =
    "Home"



--****EXTRA****
--FUNCTIONS


flip func a b =
    func b a



-- LISTS


filterOutNothing : List (Maybe a) -> List a
filterOutNothing maybes =
    List.filterMap identity maybes
