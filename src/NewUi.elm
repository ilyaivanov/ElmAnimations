module NewUi exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, img, text)
import Html.Attributes exposing (class, src)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { foo : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { foo = 12
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
view _ =
    div [ class "children" ]
        [ rowTitle "Ambient"
        , div [ class "children" ]
            [ rowTitle "Deep house"
            , div [ class "children" ]
                [ div []
                    [ rowTitle "Deep house"
                    , rowTitle "Dark house"
                    , videoTitle "Video"
                    , rowTitle "Deep true dark house"
                    ]
                ]
            , rowTitle "Dark house"
            , rowTitle "Deep true dark house"
            ]
        , viewButtons
        ]


rowTitle title =
    node title playlistIcon


videoTitle title =
    node title videoIcon


node title iconElement =
    div [ class "node-title" ]
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


viewButtons =
    div [ class "buttons" ]
        [ button [] [ text "Swap" ]
        , button [] [ text "Focus" ]
        , button [] [ text "Add" ]
        , button [] [ text "Remove" ]
        ]
