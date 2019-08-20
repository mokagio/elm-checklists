import Browser
import Html exposing (Html, div, h4, input, label, li, p, ul, text)
import Html.Attributes exposing (checked, disabled, for, id, style, type_)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initialState, update = update, view = view }

initialState =
  { steps =
    [ Step "first"
    , Step "second"
    , Step "last"
    ]
  , current = 0
  }

type alias Step =
  { name : String
  }

type Msg = MoveToNext

update msg model =
  if model.current < (List.length model.steps) then
    { steps = model.steps, current = (model.current + 1) }
  else
    model

view model =
  -- TODO: is there something like <- that I can use? instead of wrapping in ()
  div []
    [ h4 [] [text "Checklists Demo"]
    , p [] [text "Nothing of what you see is persisted ;)"]
    , div [] (List.map toListItem (process model))
    , div [] (if isCompleted model then [text "all done ✅"] else [])
    , div [style "margin-top" "40px"] [text "Next steps:"]
    , ul [] [ li [] [text "reactive style"], li [] [text "create checklist"], li [] [text "re-run checklist and track run timestamps"] ]
    ]

isCompleted model =
  model.current >= List.length model.steps

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
    -- TODO: is it safe to always move to next? what if the user somehow
    -- manages to have the checkbox checked? should that be a MoveToPrevious?
    , label [for viewModel.text, onClick MoveToNext] [text viewModel.text]
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
