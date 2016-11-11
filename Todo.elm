module Todo exposing (..)

import Html exposing (Html, a, strong, span, footer, label, button, form, header, section, text, input, li, ul, div, h1, h4)
import Html.App as App
import Html.Attributes exposing (href, checked, type', value, placeholder, class)
import Html.Events exposing (onClick, onInput, onSubmit)
import String
import List exposing (filter, length)


-- Model


type Filter
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
    , currentFilter : Filter
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
    | ChangeVisibility Filter
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
                newTodos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                { todo | isDone = not todo.isDone }
                            else
                                todo
                        )
                        model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )

        DestroyTodo id ->
            let
                newTodos =
                    List.filter (\todo -> todo.id /= id) model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )

        MakeNewTodo ->
            if not (String.isEmpty model.currentInput) then
                let
                    newTodo : Todo
                    newTodo =
                        { id = List.length model.todos, isDone = False, body = model.currentInput }
                in
                    ( { model | todos = newTodo :: model.todos, currentInput = "" }, Cmd.none )
            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "Elm Todo" ]
                , form [ onSubmit MakeNewTodo ]
                    [ input
                        [ class "new-todo"
                        , placeholder "What needs to be done?"
                        , onInput Input
                        , value model.currentInput
                        ]
                        []
                    ]
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ] (buildTodoItems model.todos) ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong []
                        [ text <| toString <| length <| filter (\t -> not t.isDone) model.todos
                        ]
                    , text " item left"
                    ]
                , ul [ class "filters" ]
                    [ li [] [ a [] [ text "All" ] ]
                    , li [] [ a [] [ text "Active" ] ]
                    , li [] [ a [] [ text "Completed" ] ]
                    ]
                ]
            ]
        , h4 [] [ text <| toString model ]
        ]


buildTodoItems : List Todo -> List (Html Msg)
buildTodoItems todos =
    let
        isTodoDone : Todo -> String
        isTodoDone { isDone } =
            if isDone then
                "completed"
            else
                ""

        makeTodoItem : Todo -> Html Msg
        makeTodoItem todo =
            li [ class (isTodoDone todo) ]
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
        List.map makeTodoItem todos



-- Main Program


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
