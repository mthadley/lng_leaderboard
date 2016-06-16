module Icon exposing (viewIcon, viewIconLabel)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Style =
    List ( String, String )


iconStyles : Style
iconStyles =
    [ ( "verticalAlign", "middle" )
    , ( "margin-right", "8px" )
    ]


labelStyles : Style
labelStyles =
    [ ( "verticalAlign", "middle" ) ]


viewIcon : String -> Html msg
viewIcon name =
    i [ class "material-icons" ] [ text name ]


viewIconLabel : String -> String -> Html msg
viewIconLabel name label =
    span [ style iconStyles ]
        [ i [ style iconStyles, class "material-icons" ] [ text name ]
        , span [ style labelStyles ] [ text label ]
        ]
