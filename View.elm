module View exposing (..)

import Html exposing (Html, a, strong, span, footer, label, button, form, header, section, text, input, li, ul, div, h1, h4)
import Html.Attributes exposing (id, autofocus, for, name, hidden, href, checked, type_, value, placeholder, class, classList)
import Html.Events exposing (onClick, onInput, onSubmit, onDoubleClick, onBlur)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Model exposing (Model, Msg(..), Todo, VisibilityFilter(..))


-- View


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ lazy buildHeader model.currentInput
            , lazy2 buildMainSection model.todos model.currentFilter
            , lazy2 buildFooter model.todos model.currentFilter
            ]
        ]


buildMainSection : List Todo -> VisibilityFilter -> Html Msg
buildMainSection todos currentFilter =
    let
        allTodosDone =
            List.all .isDone todos
    in
        section [ hidden (List.length todos == 0), class "main" ]
            [ input [ onClick ToggleAllCompleted, checked allTodosDone, class "toggle-all", type_ "checkbox", name "toggle" ] []
            , label [ for "toggle-all" ] [ text "Mark all as complete." ]
            , Keyed.ul [ class "todo-list" ] <| buildTodoItems todos currentFilter
            ]


buildHeader : String -> Html Msg
buildHeader currentInput =
    header [ class "header" ]
        [ h1 [] [ text "Elm Todo" ]
        , form [ onSubmit MakeNewTodo ]
            [ input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , onInput Input
                , value currentInput
                , autofocus True
                ]
                []
            ]
        ]


buildFooter : List Todo -> VisibilityFilter -> Html Msg
buildFooter todos currentFilter =
    let
        todosLeft =
            List.length <| List.filter (\t -> not t.isDone) todos

        todosCompleted =
            List.length todos - todosLeft
    in
        footer [ hidden (List.length todos == 0), class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text <| toString todosLeft ]
                , text " item left"
                ]
            , ul [ class "filters" ] <| List.map (buildVisibilityFilterButton currentFilter) [ All, Active, Completed ]
            , button [ onClick ClearCompleted, class "clear-completed", hidden (todosCompleted == 0) ]
                [ text <| "Clear completed (" ++ (toString todosCompleted) ++ ")" ]
            ]


buildVisibilityFilterButton : VisibilityFilter -> VisibilityFilter -> Html Msg
buildVisibilityFilterButton currentSelectedFilter desiredFilter =
    lazy2 li
        [ onClick (ChangeVisibility desiredFilter) ]
        [ a [ classList [ ( "selected", desiredFilter == currentSelectedFilter ) ] ]
            [ text <| toString desiredFilter ]
        ]


buildTodoItems : List Todo -> VisibilityFilter -> List ( String, Html Msg )
buildTodoItems todos currentFilter =
    let
        isVisible { isDone } =
            case currentFilter of
                All ->
                    True

                Active ->
                    not isDone

                Completed ->
                    isDone

        editOrView todo =
            if not todo.editing then
                div [ class "view" ]
                    [ input
                        [ class "toggle"
                        , type_ "checkbox"
                        , (checked todo.isDone)
                        , onClick (ToggleDone todo.id)
                        ]
                        []
                    , label [] [ text todo.body ]
                    , button [ class "destroy", onClick (DestroyTodo todo.id) ] []
                    ]
            else
                input
                    [ id <| "todo-" ++ (toString <| todo.id)
                    , class "edit"
                    , value todo.body
                    , onBlur (ToggleEdit todo.id)
                    , onInput EditInput
                    ]
                    []

        makeTodoItem todo =
            ( "todo-" ++ (toString <| todo.id)
            , li [ classList [ ( "completed", todo.isDone ), ( "editing", todo.editing ) ], onDoubleClick (ToggleEdit todo.id) ]
                [ editOrView todo ]
            )
    in
        List.map makeTodoItem <| List.filter isVisible todos
