module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Tree exposing (TreeItem, initialNodes)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { nodes : List TreeItem
    , focus : Focus
    }


type Focus
    = Root
    | Node String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = initialNodes, focus = Root }
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
            ( { model | focus = Node itemId }, Cmd.none )

        RemoveFocus ->
            ( { model | focus = Root }, Cmd.none )

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


view : Model -> Html Msg
view model =
    let
        flatNodes =
            case model.focus of
                Root ->
                    Tree.getNodesFlattenedWithLevels model.nodes

                Node nodeId ->
                    Tree.getChildrenFlattenedWithLevels nodeId model.nodes
    in
    div []
        [ viewHeader model
        , div [ class "page" ]
            (List.map viewNode flatNodes)
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
                            (List.map viewClickablePart parents) ++ [ viewSplitter ]

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


viewNode node =
    div
        [ style "margin-left" (String.fromInt (node.level * 20) ++ "px") ]
        [ span [ class "clickable-text", onClick (ToggleVisibility node.id) ] [ text node.title ], button [ onClick (SetFocus node.id) ] [ text "focus" ] ]
