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
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode as Json
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { mode : Mode
    , checklists : List Checklist
    }


type Mode
    = Create NewChecklistParameters
    | Run ChecklistRun
    | Browse (Maybe ChecklistRun)


init : Model
init =
    { mode = Browse Nothing
    , checklists =
        [ Checklist "numbered" [ Step "first", Step "second", Step "third" ]
        , Checklist "some and then some more" [ Step "some", Step "some more" ]
        ]
    }


type alias ChecklistRun =
    { checklist : Checklist
    , currentStep : Int
    , completed : Maybe Time.Posix
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
    | ActuallyMoveToNext Time.Posix
    | Discard
    | Select Checklist
    | CreateChecklist
    | UpdateChecklist CreateMsg
    | SaveChecklist
    | DiscardChecklist


type CreateMsg
    = UpdateTitle String
    | SaveTitle
    | UpdateStep String
    | SaveStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveToNext ->
            ( model, Task.perform ActuallyMoveToNext Time.now )

        ActuallyMoveToNext time ->
            case model.mode of
                Run checklist ->
                    let
                        stepsLength =
                            List.length checklist.checklist.steps
                    in
                    if checklist.currentStep < stepsLength then
                        let
                            nextStep =
                                checklist.currentStep + 1
                        in
                        if nextStep == stepsLength then
                            --- don't know how to get the time...
                            ( { model | mode = Run <| { checklist | completed = Just <| time, currentStep = nextStep } }
                            , Cmd.none
                            )

                        else
                            ( { model | mode = Run <| { checklist | currentStep = nextStep } }
                            , Cmd.none
                            )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Select selectedChecklist ->
            ( { model | mode = Run <| ChecklistRun selectedChecklist 0 Nothing }
            , Cmd.none
            )

        Discard ->
            ( { model | mode = Browse Nothing }
            , Cmd.none
            )

        CreateChecklist ->
            ( { model | mode = Create <| NewChecklistParameters "" (Just "") [] Nothing }
            , Cmd.none
            )

        UpdateChecklist createMsg ->
            case model.mode of
                Create parameters ->
                    ( { model | mode = Create <| updateCreate createMsg parameters }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SaveChecklist ->
            case model.mode of
                Create parameters ->
                    ( { model
                        | mode = Browse Nothing
                        , checklists = List.append model.checklists [ Checklist parameters.name parameters.steps ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DiscardChecklist ->
            ( { model | mode = Browse Nothing }, Cmd.none )


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


view : Model -> Html Msg
view model =
    div [ class "container mx-auto py-8 px-6" ]
        [ h1 [ class "text-3xl pb-2" ] [ text "Checklists Demo" ]
        , p
            [ class "italic" ]
            [ text "Nothing of what you see is persisted ;)" ]
        , div [ class "mt-4" ] [ viewModel model ]
        , div
            [ class "pt-4 pb-2" ]
            [ h3 [ class "text-xl" ] [ text "Next steps:" ] ]
        , ul
            [ class "list-disc list-inside" ]
            [ li [] [ text "re-run checklist and track run timestamps" ]
            ]
        ]



-- TODO: I don't like the name viewModel because it carries over too much
-- meaning in other contexts, I hope as the type system evolves I'll be able to
-- have clearer names here.


viewModel : Model -> Html Msg
viewModel model =
    case model.mode of
        Run checklistRun ->
            viewChecklistRun checklistRun

        Create parameters ->
            viewChecklistParameters parameters

        _ ->
            div []
                [ button
                    [ onClick CreateChecklist ]
                    [ text "❇️ Add Checklist" ]
                , viewChecklistList model.checklists
                ]


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

                placeholder_ =
                    if List.isEmpty parameters.steps then
                        "First step"

                    else
                        "Next step"
            in
            div
                []
                [ div [ class "text-lg font-medium" ] [ text parameters.name ]
                , div []
                    [ ul [] (List.map (\s -> li [] [ text s.name ]) parameters.steps)
                    ]
                , div
                    []
                    [ input
                        [ type_ "text"
                        , class "mt-2 px-1 py-2 italic border w-full"
                        , placeholder placeholder_

                        -- autofocus doesn't work on the first render (i.e.
                        -- first new step), but when this node is re-rendered
                        -- in the DOM the second time it's fine...
                        -- See also https://github.com/elm/html/issues/186
                        , autofocus True
                        , value value_
                        , onInput (\i -> UpdateChecklist <| UpdateStep i)
                        , onEnter (UpdateChecklist SaveStep)
                        ]
                        []
                    ]
                , div
                    [ class "italic text-sm w-full text-right text-gray-500" ]
                    [ text "Press Enter to add" ]
                , div [ class "text-right w-full mt-2" ]
                    [ viewDiscardChecklist
                    , button
                        [ class "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 ml-1 rounded"
                        , onClick SaveChecklist
                        ]
                        [ text "Save" ]
                    ]
                ]


viewDiscardChecklist : Html Msg
viewDiscardChecklist =
    button
        [ class "py-2 px-4 border rounded"
        , onClick DiscardChecklist
        ]
        [ text "Cancel" ]


viewChecklistParametersEmpty : String -> Html Msg
viewChecklistParametersEmpty titleValue =
    div
        []
        [ label [] [ text "What's the name of your checklist?" ]
        , input
            [ type_ "text"
            , class "mt-2 px-1 py-2 italic border w-full"
            , placeholder "My awesome checklist"
            , autofocus True
            , value titleValue
            , onInput (\i -> UpdateChecklist <| UpdateTitle i)
            , onEnter (UpdateChecklist SaveTitle)
            ]
            []
        , div
            [ class "text-right mt-2" ]
            [ viewDiscardChecklist ]
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
                [ viewCompletedRun checklistRun ]

             else
                [ a [ class "underline", onClick Discard ] [ text "Discard" ] ]
            )
        ]


viewCompletedRun : ChecklistRun -> Html msg
viewCompletedRun checklistRun =
    case checklistRun.completed of
        Nothing ->
            text "Ooops"

        Just time ->
            let
                hour =
                    String.fromInt (Time.toHour Time.utc time)

                minute =
                    String.fromInt (Time.toMinute Time.utc time)

                second =
                    String.fromInt (Time.toSecond Time.utc time)
            in
            text <| "all done ✅ by you at " ++ hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"


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
            , onCheck (\_ -> MoveToNext)
            ]
            []

        -- TODO: is it safe to always move to next? what if the user somehow
        -- manages to have the checkbox checked? should that be a
        -- MoveToPrevious?
        , label
            [ for viewData.text ]
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
