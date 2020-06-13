module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Tree exposing (Children, TreeItem, getResponses, initialNodes)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { nodes : List TreeItem
    , focused : Maybe TreeItem
    , focusedParents : List TreeItem
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = initialNodes, focused = Maybe.Nothing, focusedParents = [] }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ToggleVisibility String
    | SetFocus String
    | None


hasChild : String -> TreeItem -> Bool
hasChild id node =
    case node.children of
        Just children ->
            List.any (\sub -> sub.id == id) (getResponses children)

        Nothing ->
            False


selectParents : Model -> String -> List TreeItem -> List TreeItem
selectParents model itemId parents =
    case Tree.find (hasChild itemId) model.nodes of
        Just parent ->
            selectParents model parent.id (List.append [ parent ] parents)

        Nothing ->
            parents


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus itemId ->
            ( { model | focused = Tree.find (\i -> i.id == itemId) model.nodes, focusedParents = selectParents model itemId [] }, Cmd.none )

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


type alias TreeItemUiModel =
    { id : String
    , title : String
    , level : Int
    , parents : List TreeItem
    }


createUiModel item level parents =
    { id = item.id, title = item.title, level = level, parents = parents }


view : Model -> Html Msg
view model =
    let
        getChild : Int -> List TreeItem -> TreeItem -> List TreeItemUiModel
        getChild level parents n =
            case ( n.children, n.isVisible ) of
                ( Just children, True ) ->
                    List.append [ createUiModel n level parents ] (List.concat (List.map (getChild (level + 1) (List.append parents [ n ])) (getResponses children)))

                _ ->
                    [ createUiModel n level parents ]

        flatNodes =
            List.concat (List.map (getChild 0 []) model.nodes)
    in
    div []
        [ viewHeader model
        , div [ class "page" ]
            (List.map viewNode flatNodes)
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        currentTitle =
            Maybe.map (\m -> [ m.title ]) model.focused |> Maybe.withDefault []

        selectedPath =
            model.focusedParents |> List.map (\n -> n.title)

        titles =
            [ [ "Home" ], selectedPath, currentTitle ] |> List.concat |> String.join " > "
    in
    div [ class "header" ]
        [ text titles ]


viewNode node =
    div
        [ style "margin-left" (String.fromInt (node.level * 20) ++ "px") ]
        [ span [ onClick (ToggleVisibility node.id) ] [ text node.title ], button [ onClick (SetFocus node.id) ] [ text "focus" ] ]
