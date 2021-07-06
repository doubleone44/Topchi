module Characters

open Pinyin
open Thoth.Json

type Ideograph =
    | LeftRight
    | AboveBelow
    | LeftMiddleRight
    | AboveMiddleBelow
    | SurroundFull
    | SurroundAbove
    | SurroundBelow
    | SurroundLeft
    | SurroundRight
    | SurroundUpperLeft
    | SurroundUpperRight
    | SurroundLowerLeft
    | Overlaid

type Hint = string

let first li =
  match li with [] -> None | h::_ -> Some h

let etymologyGen (etType : string) (hint : string option) (semantic : string option) (phonetic : string option) =
    None

type Character =
    { Character: char
      Pinyin: Option<Pinyin>
      Definition: List<string>
      Decomposition: Option<Decomposition>
      Etymology: Option<Etymology>
    }

and Decomposition =
    | One of Character
    | Two of Ideograph * Option<Character> * Option<Character>
    | Three of Ideograph * Option<Character> * Option<Character> * Option<Character>

and Etymology =
    | Ideographic of Hint
    | Pictographic of Hint
    | Pictophonetic of Phonetic option * Semantic option * Hint

and Phonetic = string
and Semantic = string

type Phrase = List<Character>

type EtyString = 
    { Type: string
      Hint: string
      Semantic: Semantic option      
      Phonetic: Phonetic option }

let EtyDecoder : Decoder<EtyString> =
    Decode.object (fun get -> 
    { 
        Type = get.Optional.Field "type" Decode.string
               |> Option.defaultValue ""
        Hint = get.Optional.Field "hint" Decode.string
               |> Option.defaultValue ""
        Semantic = get.Optional.Field "semantic" Decode.string
        Phonetic = get.Optional.Field "phonetic" Decode.string
    }
)

let EtyStringToEty etyString =
    match etyString with
    | Some a -> match a.Type with
                | "ideographic" -> Some(Ideographic a.Hint) 
                | "pictographic" -> Some(Pictographic a.Hint)
                | "pictophonetic" -> Some(Pictophonetic (a.Phonetic, a.Semantic, a.Hint) )
                | _ -> None
    | None -> None

let Directions = ['⿰','⿱','⿲','⿳','⿴','⿵','⿶','⿷','⿸','⿹','⿺','⿻']



let Dec (decString : string) : Decomposition option =
    let chars = decString.ToCharArray()

    if chars.[0] = '？' then None
    else match chars.Length with
         | 1 -> Some(One(chars.[))
         | 2 ->


let decodeCharacter : Decoder<Character> =
    Decode.object (fun get ->
        { Character = (get.Required.Field "character" Decode.string).ToCharArray().[0]

          Definition = ((get.Optional.Field "definition" Decode.string
                       |> Option.defaultValue "").Split(','))
                       |> Seq.toList

          Pinyin = get.Required.Field "pinyin" (Decode.list Decode.string)
                   |> first
                   |> Option.defaultValue ""
                   |> StringToPinyin

          Decomposition = get.Optional.Field "decomposition" Decode.string
                          |> 

          Etymology = get.Optional.Field "etymology" EtyDecoder
                      |> EtyStringToEty
        })


let 手 = {
    Character = '手';
    Pinyin = StringToPinyin "shou3"
    Definition = ["hand"];
    Decomposition = Unknown;
    Etymology = Known(Pictographic("A hand with the fingers splayed"))
}

let characterJSON = System.IO.File.ReadAllText "/Data/characters.json"

let characters = Decode.Auto.fromString<List<Character>>(characterJSON)

