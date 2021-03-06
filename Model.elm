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
    , editInput : String
    }


init : List Todo -> ( Model, Cmd Msg )
init todos =
    ( Model todos All "" "", Cmd.none )



-- Message


type alias TodoID =
    Int


type Msg
    = NoOp
    | MakeNewTodo
    | ChangeVisibility VisibilityFilter
    | ToggleDone TodoID
    | ClearCompleted
    | Input String
    | EditInput String
    | DestroyTodo TodoID
    | ToggleAllCompleted
    | ToggleEdit TodoID
