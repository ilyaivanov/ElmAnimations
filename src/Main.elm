module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Tree exposing (TreeItem, getResponses, initialNodes)


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
    | RemoveFocus
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFocus itemId ->
            ( { model | focused = Tree.find (\i -> i.id == itemId) model.nodes, focusedParents = Tree.getParents model.nodes itemId }, Cmd.none )

        RemoveFocus ->
            ( { model | focused = Nothing, focusedParents = [] }, Cmd.none )

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
    }


createUiModel item level =
    { id = item.id, title = item.title, level = level }


view : Model -> Html Msg
view model =
    let
        getChild : Int -> TreeItem -> List TreeItemUiModel
        getChild level n =
            case ( n.children, n.isVisible ) of
                ( Just children, True ) ->
                    List.append [ createUiModel n level ] (List.concat (List.map (getChild (level + 1)) (getResponses children)))

                _ ->
                    [ createUiModel n level ]

        nodes =
            case model.focused of
                Just focused ->
                    focused.children |> Maybe.map getResponses |> Maybe.withDefault []

                Nothing ->
                    model.nodes

        flatNodes =
            List.concat (List.map (getChild 0) nodes)
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
            Maybe.map (\m -> m.title) model.focused |> Maybe.withDefault ""

        selectedPath =
            model.focusedParents |> List.map (\n -> [ viewSplitter, viewPart n ]) |> List.concat

        viewPart node =
            span [ class "clickable-text breadcrumb-part", onClick (SetFocus node.id) ] [ text node.title ]

        viewHome =
            span [ class "clickable-text breadcrumb-part", onClick RemoveFocus ] [ text "Home" ]

        viewNonClickablePart val =
            span [ class "breadcrumb-part" ] [ text val ]

        viewSplitter =
            span [ class "splitter" ] [ text " > " ]

        titles =
            case model.focused of
                Just _ ->
                    [ [ viewHome ], selectedPath, [ viewSplitter ], [ viewNonClickablePart currentTitle ] ] |> List.concat

                Nothing ->
                    []
    in
    div [ class "header" ]
        titles


viewNode node =
    div
        [ style "margin-left" (String.fromInt (node.level * 20) ++ "px") ]
        [ span [ class "clickable-text", onClick (ToggleVisibility node.id) ] [ text node.title ], button [ onClick (SetFocus node.id) ] [ text "focus" ] ]
