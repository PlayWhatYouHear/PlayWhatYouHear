import Html exposing (Html)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Random exposing (int, generate)
import Html.Attributes exposing (style)
import Platform.Cmd exposing (Cmd)

import MusicNotes exposing (..)

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
  , buttonPressed : Maybe Note
  , challenge : Challenge
  , numCorrect : Int
  , numMistakes : Int
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , buttonPressed = Nothing
    , challenge = Challenge <| noteFromPitchClass 1 C
    , numCorrect = 0
    , numMistakes = 0
    }, generateChallengeCmd)



-- UPDATE


type Msg
  = Tick Time
  | ButtonPress Note
  | GenerateChallenge
  | NewChallenge Challenge

generateChallengeCmd : Cmd Msg
generateChallengeCmd = Random.generate
    (NewChallenge << Challenge << noteFromMidi) (Random.int 0 119)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)
    ButtonPress note ->
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
        )
    GenerateChallenge ->
      ( { model | buttonPressed = Nothing }
      , generateChallengeCmd
      )
    NewChallenge challenge ->
      ({ model | challenge = challenge }, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (10000 * Time.millisecond) Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    myH1 s =
      Html.h1 [] [Html.text s ]

    title = myH1 "Perfect Pitch Trainer"

    myH2 s =
      Html.h2 [] [Html.text s ]

    status =
      myH2
        ("Press "
        ++ (pitchClassName
          <| pitchClassFromNote
          <| model.challenge.solution)
        ++ (case model.buttonPressed of
          Just note ->
            " - "
            ++ (pitchClassName (pitchClassFromNote note))
            ++ " Pressed"
          Nothing ->
            ""))

    noteButtons =
      List.map (noteButton << noteFromMidi) [0,1,2,3,4,5,6,7,8,9,10,11]

    score =
      renderScore model.numCorrect model.numMistakes

  in
    Html.div [] <|
      [ title
      , status
      , score
      , Html.br [] []
      , Html.button
          [ onClick GenerateChallenge
          , style [ ("font-size", "2em") ] -- #03Z9F4
          ]
          [ Html.text "New Challenge" ]
      , Html.br [] []
      ]
      ++ noteButtons

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

noteButton : Note -> Html Msg
noteButton note =
  Html.button
    [ onClick <| ButtonPress note
    , style [ ("font-size", "2em")
            , ("width", "8%")
            ]
    ]
    [ Html.text <| pitchClassName <| pitchClassFromNote <| note ]
