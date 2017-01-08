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
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , challenge = Challenge <| [ noteFromPitchClass 1 C ]
    , numChallengesDone = -1
    , meterProgress = 0
    }, generateChallengeCmd)



-- UPDATE


type Msg
  = MetTick Time
  | AnimationTick Time
  | GenerateChallenge
  | NewChallenge Challenge
  | PlayMelody
  | MelodyDone (Maybe String)

generateChallengeCmd : Cmd Msg
generateChallengeCmd = Random.generate
    (NewChallenge << Challenge) (melodyGenerator 24 48 3)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MetTick newTime ->
      ({ model | time = newTime }, Cmd.none)
    AnimationTick newTime ->
      ({ model | meterProgress = (model.meterProgress + 1) % 700 }, Cmd.none)
    GenerateChallenge ->
      ( model , generateChallengeCmd )
    NewChallenge challenge ->
      ({ model | challenge = challenge, numChallengesDone = model.numChallengesDone + 1 }, Cmd.none)
    PlayMelody ->
      (model, playMelody [Note 32])
    MelodyDone optionalMessage ->
      (model, Cmd.none)


port playMelody : List Note -> Cmd msg

-- SUBSCRIPTIONS

port melodyDone : (Maybe String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (500 * Time.millisecond) MetTick
    , Time.every (15 * Time.millisecond) AnimationTick
    , melodyDone MelodyDone 
    ]



-- VIEW
bodyStyle : Html.Attribute msg
bodyStyle = Html.Attributes.style
  [ ("background-color", "#DDDDDD")
  , ("width", "100%")
  , ("position", "absolute")
  , ("top", "0px")
  , ("left", "0px")
  , ("height", "calc(100vh)")
  , ("font-family", "sans-serif")
  ]

title : Html msg
title = Html.h1 [] [Html.text "Call and Answer Trainer"]

meter : Int -> Html msg
meter progressOutOf700 =
  svg
        [ width "700"]
        [ line [ x1 "0"
               , y1 "10"
               , x2 "700"
               , y2 "10"
               , stroke "#FFFFFF"
               , strokeWidth "5"
               ]  []
        , line [ x1 "0"
               , y1 "10"
               , x2 <| toString progressOutOf700
               , y2 "10"
               , stroke "#22DD22"
               , strokeWidth "5"
               ]  []
        , line [ x1 "0"
               , y1 "13"
               , x2 "700"
               , y2 "13"
               , stroke "#555555"
               , strokeWidth "2"
               ]  []
        ]

view : Model -> Html Msg
view model =
  let

    myH2 s =
      Html.h2 [] [Html.text s ]

    status =
      myH2 <| "Play " ++ (showMelody model.challenge.solution)

    score =
      myH2 <| toString model.numChallengesDone ++ " Completed"

  in
    Html.div 
      [ bodyStyle ] <|
      
      [ Html.div [ Html.Attributes.style [ ("margin-left", "20px") ] ] [

      [ title
      , status
      , score
      , Html.br [] []
      , Html.button 
          [ onClick PlayMelody
          , Html.Attributes.style [ ("font-size", "2em") ]
          ]
          [ Html.text "Play Melody" ]
      , Html.button
          [ onClick GenerateChallenge
          , Html.Attributes.style [ ("font-size", "2em") ]
          ]
          [ Html.text "New Challenge" ]
      , Html.br [] []
      , Html.br [] []
      ]
      ++ [ meter model.meterProgress ]]

