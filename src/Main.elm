module Main exposing (..)

{-| This is a playground experiment for me to learn
Elm and at the same time prototype a repeatable
checklist app.

Using these as reference:

  - <https://guide.elm-lang.org/architecture/>
  - <https://github.com/evancz/elm-todomvc/tree/07e3d4e5259f337d5eba781319b3a916e28aca99>

-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Task


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { mode : Maybe Mode
    , checklists : List Checklist
    }


type Mode
    = Create NewChecklistParameters
    | Run ChecklistRun


init : Model
init =
    { mode = Nothing
    , checklists =
        [ Checklist [ Step "first", Step "second", Step "third" ] "numbered"
        , Checklist [ Step "some", Step "some more" ] "some and then some more"
        ]
    }


type alias ChecklistRun =
    { checklist : Checklist
    , currentStep : Int
    }


type alias Checklist =
    { steps : List Step
    , name : String
    }


type alias Step =
    { name : String
    }


type alias NewChecklistParameters =
    { name : String
    , steps : List Step
    }


type Msg
    = MoveToNext
    | Select Checklist
    | Add
    | UpdateTitle String
    | SaveTitle
    | AddStep String
    | SaveChecklist


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveToNext ->
            case model.mode of
                Nothing ->
                    model

                Just mode ->
                    case mode of
                        Run checklist ->
                            if checklist.currentStep < List.length checklist.checklist.steps then
                                { model | mode = Just <| Run <| ChecklistRun checklist.checklist (checklist.currentStep + 1) }

                            else
                                model

                        _ ->
                            model

        Select selectedChecklist ->
            { model | mode = Just <| Run <| ChecklistRun selectedChecklist 0 }

        UpdateTitle title ->
            case model.mode of
                Nothing ->
                    model

                Just mode ->
                    case mode of
                        Create parameters ->
                            { model | mode = Just <| Create <| { parameters | name = title } }

                        _ ->
                            model

        SaveTitle ->
            case model.mode of
                Nothing ->
                    model

                Just mode ->
                    case mode of
                        Create parameters ->
                            { model | mode = Just <| Create <| { parameters | steps = [ Step "" ] } }

                        _ ->
                            model

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "container mx-auto py-8" ]
        [ h1 [ class "text-3xl pb-2" ] [ text "Checklists Demo" ]
        , p
            [ class "italic" ]
            [ text "Nothing of what you see is persisted ;)" ]
        , div [ class "py-4" ] [ viewModel model ]
        , div
            [ class "pt-4 pb-2" ]
            [ h3 [ class "text-xl" ] [ text "Next steps:" ] ]
        , ul
            [ class "list-disc list-inside" ]
            [ li [] [ text "reactive style" ]
            , li [] [ text "create checklist" ]
            , li [] [ text "re-run checklist and track run timestamps" ]
            ]
        ]



-- TODO: I don't like the name viewModel because it carries over too much
-- meaning in other contexts, I hope as the type system evolves I'll be able to
-- have clearer names here.


viewModel : Model -> Html Msg
viewModel model =
    case model.mode of
        Nothing ->
            viewChecklistList model.checklists

        Just mode ->
            case mode of
                Run checklistRun ->
                    viewChecklistRun checklistRun

                Create parameters ->
                    viewChecklistParameters parameters


viewChecklistParameters : NewChecklistParameters -> Html Msg
viewChecklistParameters parameters =
    if String.isEmpty parameters.name || List.isEmpty parameters.steps then
        viewChecklistParametersEmpty parameters

    else
        div
            []
            [ div [] [ text parameters.name ]
            , div
                []
                [ input
                    [ type_ "text"
                    , placeholder "Next step"
                    , autofocus True
                    ]
                    []
                ]
            ]


viewChecklistParametersEmpty : NewChecklistParameters -> Html Msg
viewChecklistParametersEmpty parameters =
    div
        []
        [ label [] [ text "What's the name of your checklist?" ]
        , input
            [ type_ "text"
            , placeholder "My awesome checklist"
            , autofocus True
            , onInput UpdateTitle
            , onEnter SaveTitle
            ]
            []
        ]


viewChecklistList : List Checklist -> Html Msg
viewChecklistList checklists =
    div []
        [ ul [] (List.map viewChecklistEntry checklists)
        ]


viewChecklistEntry : Checklist -> Html Msg
viewChecklistEntry checklist =
    li
        []
        [ a
            [ href "#", onClick <| Select checklist ]
            [ text <| "👉 " ++ checklist.name ]
        ]


isCompleted : ChecklistRun -> Bool
isCompleted checklistRun =
    checklistRun.currentStep >= List.length checklistRun.checklist.steps


viewChecklistRun : ChecklistRun -> Html Msg
viewChecklistRun checklistRun =
    let
        stepsViewData =
            makeViewDataForSteps checklistRun
    in
    div []
        [ div [] (List.map viewStep stepsViewData)
        , div [ class "pt-2" ]
            (if isCompleted checklistRun then
                [ text "all done ✅" ]

             else
                []
            )
        ]


type alias ChecklistStepViewData =
    { text : String
    , completed : Bool
    , active : Bool
    }


viewStep : ChecklistStepViewData -> Html Msg
viewStep viewData =
    div []
        -- TODO: using viewData.text for the id might result in
        -- inconsistencies if there are multiple steps with the same name
        [ input
            [ type_ "checkbox"
            , class "mr-2"
            , checked viewData.completed
            , disabled <| not viewData.active
            , id viewData.text
            ]
            []

        -- TODO: is it safe to always move to next? what if the user somehow
        -- manages to have the checkbox checked? should that be a
        -- MoveToPrevious?
        , label
            [ for viewData.text, onClick MoveToNext ]
            [ text viewData.text ]
        ]


makeViewDataForSteps : ChecklistRun -> List ChecklistStepViewData
makeViewDataForSteps checklistRun =
    List.indexedMap
        (\index value -> makeViewModel value index checklistRun.currentStep)
        checklistRun.checklist.steps


makeViewModel : Step -> Int -> Int -> ChecklistStepViewData
makeViewModel step index currentIndex =
    if index < currentIndex then
        { text = step.name, completed = True, active = False }

    else if index == currentIndex then
        { text = step.name, completed = False, active = True }

    else
        { text = step.name, completed = False, active = False }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
