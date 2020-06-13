module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Tree exposing (Children, TreeItem, getResponses, initialNodes)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Flags =
    {}


type alias Model =
    { nodes : List TreeItem
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { nodes = initialNodes }
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


type alias TreeItemUiModel =
    { title : String
    , level : Int
    }


view : Model -> Html Msg
view model =
    let
        getChild : Int -> TreeItem -> List TreeItemUiModel
        getChild level n =
            case n.children of
                Just children ->
                    List.append [ { title = n.title, level = level } ] (List.concat (List.map (getChild (level + 1)) (getResponses children)))

                Nothing ->
                    [ { title = n.title, level = level } ]

        flatNodes =
            List.concat (List.map (getChild 0) model.nodes)
    in
    div []
        (List.map viewNode flatNodes)


viewNode node =
    div [ style "margin-left" (String.fromInt (node.level * 20) ++ "px") ] [ text node.title ]
