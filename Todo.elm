module Todo exposing (..)

import Html
import Model exposing (Model, Msg(..), Todo, VisibilityFilter(..), init)
import Update exposing (update)
import View exposing (view)


-- Main Program


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
