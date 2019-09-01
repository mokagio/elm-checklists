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
        [ Checklist "numbered" [ Step "first", Step "second", Step "third" ]
        , Checklist "some and then some more" [ Step "some", Step "some more" ]
        ]
    }


type alias ChecklistRun =
    { checklist : Checklist
    , currentStep : Int
    }


type alias Checklist =
    { name : String
    , steps : List Step
    }


type alias Step =
    { name : String
    }


type alias NewChecklistParameters =
    { name : String
    , editingName : Maybe String
    , steps : List Step
    , editingStep : Maybe Step
    }


type Msg
    = MoveToNext
    | Select Checklist
    | CreateChecklist CreateMsg


type CreateMsg
    = UpdateTitle String
    | SaveTitle
    | UpdateStep String
    | SaveStep
    | Done


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

        CreateChecklist createMsg ->
            case model.mode of
                Nothing ->
                    model

                Just mode ->
                    case mode of
                        Create parameters ->
                            { model | mode = Just <| Create <| updateCreate createMsg parameters }

                        _ ->
                            model


updateCreate : CreateMsg -> NewChecklistParameters -> NewChecklistParameters
updateCreate msg model =
    case msg of
        UpdateTitle newTitle ->
            { model | editingName = Just newTitle }

        SaveTitle ->
            case model.editingName of
                Nothing ->
                    model

                Just editingName ->
                    { model | name = editingName, editingName = Nothing }

        UpdateStep name ->
            { model | editingStep = Just <| Step name }

        SaveStep ->
            case model.editingStep of
                Nothing ->
                    model

                Just editingStep ->
                    { model
                        | steps = List.append model.steps [ editingStep ]
                        , editingStep = Nothing
                    }

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
    case parameters.editingName of
        Just name ->
            viewChecklistParametersEmpty name

        Nothing ->
            let
                value_ =
                    case parameters.editingStep of
                        Nothing ->
                            ""

                        Just step ->
                            step.name
            in
            div
                []
                [ div [] [ text parameters.name ]
                , div []
                    [ ul [] (List.map (\s -> li [] [ text s.name ]) parameters.steps)
                    ]
                , div
                    []
                    [ input
                        [ type_ "text"
                        , placeholder "Next step"
                        , autofocus True -- this doesn't seem to work, but only the very first time the node renders (i.e. the first step)
                        , value value_
                        , onInput (\i -> CreateChecklist <| UpdateStep i)
                        , onEnter (CreateChecklist SaveStep)
                        ]
                        []
                    ]
                ]


viewChecklistParametersEmpty : String -> Html Msg
viewChecklistParametersEmpty titleValue =
    div
        []
        [ label [] [ text "What's the name of your checklist?" ]
        , input
            [ type_ "text"
            , placeholder "My awesome checklist"
            , autofocus True
            , value titleValue
            , onInput (\i -> CreateChecklist <| UpdateTitle i)
            , onEnter (CreateChecklist SaveTitle)
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
