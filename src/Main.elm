import Browser
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, disabled, for, id, type_)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initialState, update = update, view = view }

initialState =
  { steps =
    [ Step "first"
    , Step "second"
    , Step "last"
    ]
  , current = 1
  }

type alias Step =
  { name : String
  }

update msg model =
  model

view model =
  -- TODO: is there something like <- that I can use? instead of wrapping in ()
  div [] (List.map toListItem (process model))

toListItem viewModel =
  div []
    -- TODO: using viewModel.text for the id might result in inconsistencies if
    -- there are multiple steps with the same name
    [ input
      [ type_ "checkbox"
      , checked viewModel.completed
      , disabled <| not viewModel.active
      , id viewModel.text
      ] []
    , label [for viewModel.text] [text viewModel.text]
    ]

process state =
  List.indexedMap
    -- TODO: figure out the Elm Html way of doing strikthrough, if any, or add
    -- an attribute to the element
    (\index value -> makeViewModel value index state.current)
    state.steps

makeViewModel step index currentIndex =
  if index < currentIndex then
    { text = step.name, completed = True, active = False }
  else if index == currentIndex then
    { text = step.name, completed = False, active = True }
  else
    { text = step.name, completed = False, active = False }
