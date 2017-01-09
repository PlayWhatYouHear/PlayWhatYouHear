port module CallAnswerTrainer exposing (..)
{- Description

App flow:
  Short line plays with prompt for computer.
  Equal time devoted to player with prompt for player.
  Button to generate next challenge.

Possible next step: onscreen piano.
-}

import Html exposing (Html)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Random exposing (int, generate)
import Html.Attributes exposing (style)
import Platform.Cmd exposing (Cmd)
import Svg exposing (line, svg)
import Svg.Attributes exposing (..)
import MusicNotes exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Challenge =
  { solution : List Note
  }

type alias Model =
  { time : Time
  , challenge : Challenge
  , numChallengesDone : Int
  , meterProgress : Int
  , showSolution : Bool
  , userStarted : Bool
  , beat : Int
  , beating : Bool
  , muted : Bool
  , notesPerChallenge : Int
  , numNotesGenerated : Int
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , challenge = Challenge <| [ noteFromPitchClass C 1 ]
    , numChallengesDone = -1
    , meterProgress = 0
    , showSolution = False
    , userStarted = False
    , beat = 0
    , beating = False
    , muted = False
    , notesPerChallenge = 3
    , numNotesGenerated = 3
    }, generateChallengeCmd 3)



-- UPDATE

type Msg
  = MetTick Time
  | GenerateChallenge
  | NewChallenge Challenge
  | ShowSolution
  | Start
  | StopMelody
  | ToggleMute

generateChallengeCmd : Int -> Cmd Msg
generateChallengeCmd notesPerMelody = Random.generate
    (NewChallenge << Challenge) (diatonicMelodyGenerator 4 7 notesPerMelody)

playNoteCmdSelector : Model -> Cmd Msg
playNoteCmdSelector model =
  let
    listGet index list =
      if (List.length list) >= index
      then List.take index list |> List.reverse |> List.head
      else Nothing

    note = listGet (model.beat + 1) model.challenge.solution
  in
    if model.beating && (not model.muted)
    then case note of
      Nothing -> Cmd.none
      Just n -> playNote n -- make it play notes here
    else Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MetTick newTime ->
      ({ model 
        | time = newTime
        , beat =
            if model.beating
            then (model.beat + 1) % 8
            else model.beat
        }
        , playNoteCmdSelector model
        )

    GenerateChallenge ->
      ( { model | numNotesGenerated = model.numNotesGenerated + model.notesPerChallenge }
      , generateChallengeCmd model.notesPerChallenge)

    NewChallenge challenge ->
      ({ model 
          | challenge = challenge
          , numChallengesDone = model.numChallengesDone + 1
          , showSolution = False
          , beat = 0
          }, Cmd.none)

    ShowSolution ->
      ({ model | showSolution = True }, Cmd.none)

    Start ->
      ({model
        | userStarted = True
        , beating = True
        }
        , Cmd.none)

    StopMelody ->
      (model, stopMelody ())
    
    ToggleMute ->
      ({ model | muted = not model.muted } , Cmd.none )

port stopMelody : () ->  Cmd msg
port playNote : Note -> Cmd msg
port reportMetrics : Int -> Cmd msg

-- SUBSCRIPTIONS

port melodyDone : (Maybe String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (500 * Time.millisecond) MetTick
    ]



-- VIEW
bodyStyle : Html.Attribute msg
bodyStyle = Html.Attributes.style
  [ ("width", "100%")
  , ("position", "absolute")
  , ("top", "0px")
  , ("left", "0px")
  , ("height", "calc(100vh)")
  , ("font-family", "sans-serif")
  , ("text-align", "center")
  ]

title : Html msg
title = Html.h1
  [ Html.Attributes.style 
      [ --("text-align","center")
      ] 
  ]
  [Html.text "Play What You Hear"]

meter : Int -> Html msg
meter progressOutOf400 =
  svg
    [ width "400"
    , height "20"
    , Html.Attributes.style [ ("display","block") ]
    ]
    [ line  [ x1 "0"
            , y1 "10"
            , x2 "400"
            , y2 "10"
            , stroke "#FFFFFF"
            , strokeWidth "5"
            ]  []
    , line  [ x1 "0"
            , y1 "10"
            , x2 <| toString progressOutOf400
            , y2 "10"
            , stroke "#22DD22"
            , strokeWidth "5"
            ]  []
    , line  [ x1 "0"
            , y1 "13"
            , x2 "400"
            , y2 "13"
            , stroke "#555555"
            , strokeWidth "2"
            ]  []
    ]

--metGraphic beat =

materialButton : String -> msg -> List (String, String)-> Html msg
materialButton text click styles =
  let
    class = Html.Attributes.class
  in
    Html.button 
      [ onClick click
      , Html.Attributes.style 
          ([ ("font-size", "2em")
          ] ++ styles)
      , class "btn"
      , class "blue"
      ]
      [ Html.text text ]

view : Model -> Html Msg
view model =
  let
    appContent =
      if model.userStarted
      then runningAppScreen model
      else startScreen model

    pageWrapper trans =
      Html.div [ bodyStyle, Html.Attributes.style [{-("margin-left", "5px")-}] ] [ title, trans ]
    
    appWrapper trans =
      Html.div [ Html.Attributes.style [("margin-left","15px"), ("margin-right","15px")] ] [ trans ]

  in
    pageWrapper <| appWrapper <| appContent

startScreen : Model -> Html Msg
startScreen model =
  Html.div
    [ ]
    [ Html.p 
        [] 
        [ Html.text <|
            "Play what you hear on your instrument."
            ++ " This is an exercise to link what"
            ++ " you hear in your head with how you"
            ++ " play your instrument."
            ]
    , materialButton "Play" Start []
    ]

runningAppScreen : Model -> Html Msg
runningAppScreen model =
  let
    myH2 s =
      Html.h2 [] [Html.text s ]

    solution =
      if model.showSolution
      then
        let 
          noteDiv note =
            Html.div 
              [ Html.Attributes.style 
                  [ ("display", "inline")
                  , ("margin-right","15px")
                  , ("font-size", "3em")
                  ]
              ] 
              [ Html.text <| noteName note ]
        in
          Html.div [ ] (List.map noteDiv model.challenge.solution )
      else
        materialButton "Show Solution" ShowSolution [ ("margin-top", "10") ]

    score =
      myH2 <| toString model.numChallengesDone ++ " Completed"
    
    notesGenerated =
      myH2 <| toString model.numNotesGenerated ++ " Notes Generated"

    br =
      Html.br [] []

  in

    Html.div 
      [ ]
      [
       materialButton "Next Challenge" GenerateChallenge [ ("margin-left","10px") ]
      , score
      , solution
      , br
      , br
      , br
      , Html.button
          [ onClick ToggleMute, Html.Attributes.class "grey", Html.Attributes.class "btn" ]
          [ Html.text <| if model.muted then "Unmute" else "Mute" ]
      ]