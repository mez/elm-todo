module Model exposing (..)

-- Model


type VisibilityFilter
    = All
    | Active
    | Completed


type alias Todo =
    { id : Int
    , isDone : Bool
    , body : String
    , editing : Bool
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
    | ToggleAllCompleted
    | EditTodo TodoID
