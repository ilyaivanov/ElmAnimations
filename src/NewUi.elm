module NewUi exposing (..)

import Assets
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import ExtraEvents exposing (classIf, emptyElement)
import Html exposing (Attribute, Html, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src)
import Html.Events exposing (onClick)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { tree : HashTree
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree = sampleData
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = None
    | Toggle String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle id ->
            ( { model | tree = toggleVisibility model.tree id }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ viewSidebar model.tree, viewTree, viewSearch ]


viewSidebar treeHash =
    div [ class "sidebar" ]
        [ div [ class "sidebar-items-children" ]
            (getHomeItems treeHash |> List.map (viewSidebarItem treeHash))
        ]


viewSidebarItem : HashTree -> TreeItem -> Html Msg
viewSidebarItem treeHash item =
    div []
        [ div [ class "sidebar-item" ]
            [ div
                [ onClick (Toggle item.id)
                , class "sidebar-item-chevron"
                , classIf item.isVisible "open"
                ]
                [ img [ src Assets.chevron ] [] ]
            , div [] [ text item.title ]
            ]
        , if item.isVisible then
            getChildren treeHash item.id |> viewChildren treeHash

          else
            emptyElement
        ]


viewChildren treeHash childs =
    div [ class "sidebar-items-children" ]
        (List.map (viewSidebarItem treeHash)
            childs
        )


viewSearch =
    div [ class "search" ]
        [ input [ placeholder "Search for videos, channels", class "search-input" ] []
        , tree
        ]


viewTree =
    div [ class "tree" ]
        [ tree ]


tree =
    div [ class "children" ]
        [ rowTitle "Ambient"
        , div [ class "children" ]
            [ rowTitle "Deeply house"
            , div [ class "children" ]
                [ div []
                    [ rowTitle "Deep house"
                    , rowTitle "Dark house"
                    , videoTitle "Video"
                    , rowTitle "Deep true dark house"
                    ]
                ]
            , rowTitle "Dark house"
            , div [ class "children" ]
                [ videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , videoTitle "Video"
                , rowTitle "Nested Dark house"
                , div [ class "children" ]
                    [ videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    , videoTitle "Video"
                    ]
                ]
            , rowTitle "Deep true dark house"
            , rowTitle "Deep true dark house"
            , rowTitle "Deep true dark house"
            , rowTitle "Deep true dark house"
            , rowTitle "Deep true dark house"
            , rowTitle "Deep true dark house"
            ]
        ]


rowTitle title =
    node title playlistIcon False


videoTitle title =
    node title videoIcon False


node title iconElement isHidden =
    div [ class "node-title", classIf isHidden "hidden" ]
        [ div [ class "branch" ] []
        , div [ class "branch-bubble" ] []
        , iconElement
        , div [ class "node-title-text" ] [ text title ]
        , div [ class "row-icon" ] [ text "X" ]
        ]


videoIcon =
    img [ src "https://i.ytimg.com/vi/gmQJVl51yCc/mqdefault.jpg", class "image" ] []


playlistIcon =
    div [ class "circle" ] []



--TREE STRUCTRUE
--type HashTree =


type alias TreeItem =
    { id : String
    , title : String
    , isVisible : Bool
    , children : List String
    }


type alias HashTree =
    Dict String TreeItem


sampleData =
    Dict.fromList
        [ ( home, TreeItem home home True [ "1", "2" ] )
        , ( "1", TreeItem "1" "Ambient" True [ "1.1", "1.2", "1.3" ] )
        , ( "1.1", TreeItem "1.1" "Ambient Child 1" False [] )
        , ( "1.2", TreeItem "1.2" "Ambient Child 2" False [] )
        , ( "1.3", TreeItem "1.3" "Ambient Child 3" False [] )
        , ( "2", TreeItem "2" "Deep House" False [] )
        ]


getHomeItems : HashTree -> List TreeItem
getHomeItems hasTree =
    getChildren hasTree home


getChildren : HashTree -> String -> List TreeItem
getChildren hashTree nodeId =
    let
        childrenIds =
            hashTree
                |> Dict.get nodeId
                |> Maybe.map (\i -> i.children)
                |> Maybe.withDefault []
    in
    childrenIds
        |> List.map (\id -> Dict.get id hashTree)
        |> filterOutNothing


filterOutNothing : List (Maybe a) -> List a
filterOutNothing maybes =
    List.filterMap identity maybes


toggleVisibility : HashTree -> String -> HashTree
toggleVisibility hash id =
    let
        itemM =
            Dict.get id hash
    in
    case itemM of
        Just item ->
            Dict.insert id { item | isVisible = not item.isVisible } hash

        Nothing ->
            hash


home =
    "HOME"
