import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initialState, update = update, view = view }

initialState =
  [ Step "first"
  , Step "second"
  , Step "last"
  ]

type alias Step =
  { name : String
  }

update msg model =
  model

view model =
  -- TODO: is there something like <- that I can use? instead of wrapping in ()
  div [] (List.map stepToHTML model)

stepToHTML step =
  div [] [ text step.name ]
