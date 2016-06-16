module Main exposing (..)

import Html.App as Html
import App


main : Program Never
main =
    Html.program
        { init = App.init
        , view = App.view
        , update = App.update
        , subscriptions = App.subscriptions
        }
