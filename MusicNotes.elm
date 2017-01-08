module MusicNotes exposing (..)

import Random exposing (Generator, int, list)

type alias Note = { midiNumber : MidiNumber }
type alias Octave = Int
type alias MidiNumber = Int

noteGenerator : MidiNumber -> MidiNumber -> Generator Note
noteGenerator low high = Random.map Note (int low high)

diatonicNoteGenerator : PitchClass -> Octave -> Int -> Generator Note
diatonicNoteGenerator ionicKey octave numNotes =
  let
    startingMidi = midiFromPitchClass ionicKey octave
      
    intervalGenerator =
      int 0 <| numNotes - 1
    
    midiFromInterval interval =
      startingMidi + (12 * (interval // 7)) + case interval % 7 of
        0 -> 0
        1 -> 2
        2 -> 4
        3 -> 5
        4 -> 7
        5 -> 9
        6 -> 11
        _ -> 0

    noteFromInterval =
      Note << midiFromInterval

  in
    Random.map noteFromInterval intervalGenerator

pitchClassGenerator : Generator PitchClass
pitchClassGenerator =
  Random.map pitchClassFromMidi <| int 0 10


melodyGenerator : MidiNumber -> MidiNumber -> Int -> Generator (List Note)
melodyGenerator low high length = list length <| noteGenerator low high

keyedDiatonicMelodyGenerator : PitchClass -> Octave -> Int -> Int -> Generator (List Note)
keyedDiatonicMelodyGenerator ionicKey octave numNotes length =
  list length <| diatonicNoteGenerator ionicKey octave numNotes

diatonicMelodyGenerator : Octave -> Int -> Int -> Generator (List Note)
diatonicMelodyGenerator octave numNotes length =
  Random.andThen (\pc -> keyedDiatonicMelodyGenerator pc octave numNotes length) pitchClassGenerator


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

midiClass : PitchClass -> MidiNumber
midiClass pitchClass =
  case pitchClass of
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

midiFromPitchClass : PitchClass -> Octave -> MidiNumber
midiFromPitchClass pitchClass octave =
  (midiClass pitchClass) + (12 * octave)

noteFromPitchClass : PitchClass -> Octave -> Note
noteFromPitchClass pitchClass octave =
  Note <| midiFromPitchClass pitchClass octave

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

showMelody : List Note -> String
showMelody melody =
  List.foldr (\a s -> a ++ " " ++ s ) "" <| List.map noteLongName melody