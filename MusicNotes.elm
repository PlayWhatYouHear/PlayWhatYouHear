module MusicNotes exposing (..)

import Random exposing (Generator, int, list)

type alias Note = { midiNumber : MidiNumber }
type alias Octave = Int
type alias MidiNumber = Int

noteGenerator : Int -> Int -> Generator Note
noteGenerator low high = Random.map Note (int low high)

melodyGenerator : Int -> Int -> Int -> Generator (List Note)
melodyGenerator low high length = list length <| noteGenerator low high

pitchClassFromMidi : MidiNumber -> PitchClass
pitchClassFromMidi midi =
  case midi % 12 of
    0  -> C
    1  -> Cs
    2  -> D
    3  -> Ds
    4  -> E
    5  -> F
    6  -> Fs
    7  -> G
    8  -> Gs
    9  -> A
    10 -> As
    _  -> B

pitchClassFromNote : Note -> PitchClass
pitchClassFromNote note =
    pitchClassFromMidi note.midiNumber

noteFromMidi : MidiNumber -> Note
noteFromMidi midiNumber =
    Note midiNumber

noteFromPitchClass : Octave -> PitchClass -> Note
noteFromPitchClass octave pitchClass =
  let
    midiClass = case pitchClass of
      C  -> 0
      Cs -> 1
      D  -> 2
      Ds -> 3
      E  -> 4
      F  -> 5
      Fs -> 6
      G  -> 7
      Gs -> 8
      A  -> 9
      As -> 10
      B  -> 11
  in
    Note <| midiClass + (12 * octave)

type PitchClass
  = C | Cs | D | Ds | E | F
  | Fs | G | Gs | A | As | B

pitchClassName : PitchClass -> String
pitchClassName pitchClass =
  case pitchClass of
    C  -> "C"
    Cs -> "C#"
    D  -> "D"
    Ds -> "D#"
    E  -> "E"
    F  -> "F"
    Fs -> "F#"
    G  -> "G"
    Gs -> "G#"
    A  -> "A"
    As -> "A#"
    B  -> "B"

noteName : Note -> String
noteName note = pitchClassName <| pitchClassFromNote note

noteLongName : Note -> String
noteLongName note =
  (pitchClassName <| pitchClassFromNote note)
  ++ (toString (if note.midiNumber == 0
      then 0
      else note.midiNumber // 12))
