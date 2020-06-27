module NewUi exposing (..)

import Browser
import ExtraEvents exposing (classIf)
import Html exposing (Attribute, Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Ports


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { isDeepHouseVisible : Bool
    , focused : FocusedNode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isDeepHouseVisible = True
      , focused = NoNode
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = Focus FocusedNode
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus focus ->
            ( { model | focused = focus }, Ports.scrollToTop () )

        None ->
            ( model, Cmd.none )


type FocusedNode
    = NoNode
    | DarkHouse
    | NestedDarkHouse


view : Model -> Html Msg
view model =
    let
        ( top, left ) =
            case model.focused of
                NoNode ->
                    ( "0", "0" )

                DarkHouse ->
                    ( "-300px", "-50px" )

                NestedDarkHouse ->
                    ( "-800px", "-75px" )
    in
    div [ class "page", style "margin-top" top, style "margin-left" left ]
        [ div [ class "children" ]
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
            , viewButtons
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


viewButtons =
    div [ class "buttons" ]
        [ text "Focus"
        , button [ onClick (Focus NoNode) ] [ text "Root" ]
        , button [ onClick (Focus DarkHouse) ] [ text "Dark house" ]
        , button [ onClick (Focus NestedDarkHouse) ] [ text "Nested Dark house" ]
        ]
