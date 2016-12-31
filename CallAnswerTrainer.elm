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
  { solution : Note
  }

type alias Model =
  { time : Time
  --, buttonPressed : Maybe Note
  , challenge : Challenge
  , numCorrect : Int
  , numMistakes : Int
  , meterProgress : Int
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    --, buttonPressed = Nothing
    , challenge = Challenge <| noteFromPitchClass 1 C
    , numCorrect = 0
    , numMistakes = 0
    , meterProgress = 0
    }, generateChallengeCmd)



-- UPDATE


type Msg
  = MetTick Time
  | AnimationTick Time
  --| ButtonPress Note
  | GenerateChallenge
  | NewChallenge Challenge
  | PlayMelody

generateChallengeCmd : Cmd Msg
generateChallengeCmd = Random.generate
    (NewChallenge << Challenge << noteFromMidi) (Random.int 0 119)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MetTick newTime ->
      ({ model | time = newTime }, Cmd.none)
    AnimationTick newTime ->
      ({ model | meterProgress = (model.meterProgress + 1) % 700 }, Cmd.none)
    --ButtonPress note ->
    --  handleButtonPress msg model note
    GenerateChallenge ->
      ( model--( { model | buttonPressed = Nothing }
      , generateChallengeCmd
      )
    NewChallenge challenge ->
      ({ model | challenge = challenge }, Cmd.none)
    PlayMelody ->
      (model, playMelody [Note 32])

{-handleButtonPress : Msg -> Model -> Note -> (Model, Cmd Msg)
handleButtonPress msg model note =
  let
    solved =
      (pitchClassFromNote note)
      == (pitchClassFromNote model.challenge.solution)

    buttonPressed =
      if solved
      then Nothing
      else Just note

    numCorrect =
      if solved
      then model.numCorrect + 1
      else model.numCorrect

    numMistakes =
      if solved
      then model.numMistakes
      else model.numMistakes + 1

    command =
      if solved
      then generateChallengeCmd
      else Cmd.none
  in
    ({ model
      | buttonPressed = buttonPressed
      , numCorrect = numCorrect
      , numMistakes = numMistakes
      }
    , command
    )-}


port playMelody : List Note -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (500 * Time.millisecond) MetTick
    , Time.every (15 * Time.millisecond) AnimationTick
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
  ]

title = Html.h1 [] [Html.text "Call and Answer Trainer"]

view : Model -> Html Msg
view model =
  let

    myH2 s =
      Html.h2 [] [Html.text s ]

    status =
      myH2
        ("Press "
        ++ (pitchClassName
          <| pitchClassFromNote
          <| model.challenge.solution)
        {-++ (case model.buttonPressed of
          Just note ->
            " - "
            ++ (pitchClassName (pitchClassFromNote note))
            ++ " Pressed"
          Nothing ->
            "")-})

    {-noteButtons =
      List.map (noteButton << noteFromMidi) [0,1,2,3,4,5,6,7,8,9,10,11]-}

    score =
      renderScore model.numCorrect model.numMistakes

  in
    Html.div [ bodyStyle ] <|
      [ title
      , status
      --, score
      , Html.br [] []
      , Html.button [onClick PlayMelody] [ Html.text "Play Melody" ]
      , Html.button
          [ onClick GenerateChallenge
          , Html.Attributes.style [ ("font-size", "2em") ] -- #03Z9F4
          ]
          [ Html.text "New Challenge" ]
      , Html.br [] []
      , Html.br [] []
      ]
      ++ [
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
               , x2 <| toString model.meterProgress
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
      ]

renderScore : Int -> Int -> Html Msg
renderScore numCorrect numMistakes =
  Html.h2
    []
    [ Html.text
      (toString
        numCorrect
        ++ "/"
        ++ toString (numMistakes + numCorrect))
    ]

{-noteButton : Note -> Html Msg
noteButton note =
  Html.button
    [ onClick <| ButtonPress note
    , Html.Attributes.style [ ("font-size", "2em")
            , ("width", "8%")
            ]
    ]
    [ Html.text <| pitchClassName <| pitchClassFromNote <| note ]-}
