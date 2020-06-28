module NewUi exposing (..)

import Browser
import ExtraEvents exposing (classIf)
import Html exposing (Attribute, Html, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { foo : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { foo = 1
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ viewSidebar, viewTree, viewSearch ]


viewSidebar =
    div [ class "sidebar" ] [ text "Sidebar" ]


viewSearch =
    div [ class "search" ]
        [  input [placeholder "Search for videos, channels", class "search-input"] []
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
