port module Todo exposing (..)

import Html
import Model exposing (Model, Msg(..), Todo, VisibilityFilter(..), init)
import Update exposing (update)
import View exposing (view)


-- Main Program


port setStorage : List Todo -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel.todos, cmds ]
        )


main : Program (List Todo) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = (always Sub.none)
        }
