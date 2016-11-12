module Todo exposing (..)

import Html exposing (Html, a, strong, span, footer, label, button, form, header, section, text, input, li, ul, div, h1, h4)
import Html.App as App
import Html.Attributes exposing (hidden, href, checked, type', value, placeholder, class, classList)
import Html.Events exposing (onClick, onInput, onSubmit)
import String
import List exposing (filter, length, map)


-- Model


type VisibilityFilter
    = All
    | Active
    | Completed


type alias Todo =
    { id : Int
    , isDone : Bool
    , body : String
    }


type alias Model =
    { todos : List Todo
    , currentFilter : VisibilityFilter
    , currentInput : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] All "", Cmd.none )



-- Message


type alias TodoID =
    Int


type Msg
    = MakeNewTodo
    | ChangeVisibility VisibilityFilter
    | ToggleDone TodoID
    | ClearCompleted
    | Input String
    | DestroyTodo TodoID



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            ( { model | currentInput = str }, Cmd.none )

        ToggleDone id ->
            let
                findAndToggleTodo todo =
                    if todo.id == id then
                        { todo | isDone = not todo.isDone }
                    else
                        todo

                updatedTodos =
                    List.map findAndToggleTodo model.todos
            in
                ( { model | todos = updatedTodos }, Cmd.none )

        DestroyTodo id ->
            let
                updatedTodos =
                    List.filter (\todo -> todo.id /= id) model.todos
            in
                ( { model | todos = updatedTodos }, Cmd.none )

        MakeNewTodo ->
            if not (String.isEmpty model.currentInput) then
                let
                    newTodo =
                        { id = List.length model.todos, isDone = False, body = model.currentInput }
                in
                    ( { model | todos = newTodo :: model.todos, currentInput = "" }, Cmd.none )
            else
                ( model, Cmd.none )

        ChangeVisibility desiredFilter ->
            ( { model | currentFilter = desiredFilter }, Cmd.none )

        ClearCompleted ->
            let
                updatedTodos =
                    List.filter (\t -> not t.isDone) model.todos
            in
                ( { model | todos = updatedTodos }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ buildHeader model.currentInput
            , section [ class "main" ]
                [ ul [ class "todo-list" ] <| buildTodoItems model.todos model.currentFilter ]
            , buildFooter model.todos model.currentFilter
            ]
        , h4 [] [ text <| toString model ]
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
        footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text <| toString todosLeft ]
                , text " item left"
                ]
            , ul [ class "filters" ] <| List.map (buildVisibilityFilterButton currentFilter) [ All, Active, Completed ]
            , button [ onClick ClearCompleted, class "clear-completed", hidden (todosCompleted == 0) ] [ text <| "Clear completed (" ++ (toString todosCompleted) ++ ")" ]
            ]


buildVisibilityFilterButton : VisibilityFilter -> VisibilityFilter -> Html Msg
buildVisibilityFilterButton currentSelectedFilter desiredFilter =
    li [ onClick (ChangeVisibility desiredFilter) ]
        [ a [ classList [ ( "selected", desiredFilter == currentSelectedFilter ) ] ]
            [ text <| toString desiredFilter ]
        ]


buildTodoItems : List Todo -> VisibilityFilter -> List (Html Msg)
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

        makeTodoItem todo =
            li [ classList [ ( "completed", todo.isDone ) ] ]
                [ div [ class "view" ]
                    [ input
                        [ class "toggle"
                        , type' "checkbox"
                        , (checked todo.isDone)
                        , onClick (ToggleDone todo.id)
                        ]
                        []
                    , label [] [ text todo.body ]
                    , button [ class "destroy", onClick (DestroyTodo todo.id) ] []
                    ]
                ]
    in
        List.map makeTodoItem <| List.filter isVisible todos



-- Main Program


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
