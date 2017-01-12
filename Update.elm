module Update exposing (..)

import Model exposing (Msg(..), Model)


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
                        { id = List.length model.todos
                        , isDone = False
                        , body = model.currentInput
                        , editing = False
                        }
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

        ToggleAllCompleted ->
            let
                -- first we need to check if all are already completed.
                allTodosDone =
                    List.all .isDone model.todos

                -- now go through todos and do opposite
                updatedTodos =
                    List.map (\t -> { t | isDone = not allTodosDone }) model.todos
            in
                ( { model | todos = updatedTodos }, Cmd.none )

        EditTodo id ->
            let
                findAndSetEditingTodo todo =
                    if todo.id == id then
                        { todo | editing = True }
                    else
                        todo

                updatedTodos =
                    List.map findAndSetEditingTodo model.todos
            in
                ( { model | todos = updatedTodos }, Cmd.none )
