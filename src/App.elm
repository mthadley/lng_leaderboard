module App exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events exposing (onClick)
import Http
import Icon exposing (..)
import Json.Decode exposing (..)
import Result
import String
import Task
import Time

-- MODEL

type SortOrder = Score | Name

type alias Entry =
  { name : String
  , id : String
  , loopid : Int
  , gamesPlayed : Int
  , gamesWon : Int
  }

type alias Model =
  { scores : List Entry
  , sortOrder : SortOrder
  }

init : (Model, Cmd Msg)
init = (Model [] Score, getScores)

getWinP : Entry -> Float
getWinP entry =
  case entry.gamesPlayed of
    0 -> 0
    _ -> (toFloat entry.gamesWon) / (toFloat entry.gamesPlayed)

sortByScore : Entry -> Entry -> Order
sortByScore a b = compare (getWinP b) (getWinP a)

sortByName : Entry -> Entry -> Order
sortByName a b = compare a.name b.name

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "row" ]
      [ div [ class "col s12" ]
        [ viewNav model
        , table [ class "centered striped z-depth-1" ]
          [ tbody [] <| viewScores model ]
        , viewKey
        ]
      ]
    ]

viewKey : Html Msg
viewKey =
  p []
    [ viewIconLabel "perm_identity" "= Name"
    , viewIconLabel "info" "= Score %"
    ]

viewNav : Model -> Html Msg
viewNav model =
  nav []
    [ div [ class "col s12" ]
      [ div [ class "nav-wrapper" ]
        [ a [ href "#", class "brand-logo" ]
            [ text "Leaderboard" ]
        , viewSortDropdown model.sortOrder
        , span [ class "right" ]
          [ text <| String.append "Players: " <| toString <| List.length model.scores ]
        ]
      ]
    ]

viewScores : Model -> List (Html Msg)
viewScores model =
  let
    comparator =
      case model.sortOrder of
        Score -> sortByScore
        Name -> sortByName
  in
    List.indexedMap viewScore <| List.sortWith comparator model.scores

viewScore : Int -> Entry -> Html Msg
viewScore i entry =
  tr []
    [ td [] [ span [] [ text <| toString (i + 1) ] ]
    , td [] [ viewIconLabel "perm_identity" entry.name ]
    , td [] [ viewIconLabel "info" <| String.left 4 <| toString <| getWinP entry ]
    ]

viewSortIcon : SortOrder -> Html Msg
viewSortIcon order =
  case order of
    Name -> viewIcon "perm_identity"
    Score -> viewIcon "info"

viewSortDropdown : SortOrder -> Html Msg
viewSortDropdown order =
  div [ class "fixed-action-btn" ]
    [ a [ class "btn-floating btn-large blue" ] [ viewSortIcon order ]
    , ul []
      [ li []
        [ a [ onClick (ChangeSort Name), class "btn-floating" ]
          [ viewSortIcon Name ]
        ]
      , li []
        [ a [ onClick (ChangeSort Score), class "btn-floating" ]
          [ viewSortIcon Score ]
        ]
      ]
    ]

-- UPDATE

type Msg
  = Noop
  | ChangeSort SortOrder
  | GetScores
  | GetScoresSuccess (List Entry)
  | GetScoresFailure Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)

    ChangeSort order ->
      ({model | sortOrder = order}, Cmd.none)

    GetScores ->
      (model, getScores)

    GetScoresSuccess scores ->
      ({model | scores = scores}, Cmd.none)

    GetScoresFailure _ ->
      (model, Cmd.none)

-- REQUEST

apiUrl : String
apiUrl = "http://liferay.io/loopgame/players/"

getScores : Cmd Msg
getScores =
    Task.perform GetScoresFailure GetScoresSuccess (Http.get decodeScores apiUrl)

decodeScores : Decoder (List Entry)
decodeScores =
  list decodeEntry

decodeEntry : Decoder Entry
decodeEntry =
  object5 Entry
    ("name" := string)
    ("id" := string)
    ("loopId" := int)
    ("gamesPlayed" := int)
    ("gamesWon" := int)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (2 * Time.second) (\t -> GetScores)
