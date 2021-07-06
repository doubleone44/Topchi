module Pinyin

open System
open System.Text.RegularExpressions

type ToneDescription =
    | High
    | Rising
    | Dipping
    | Falling
    | Neutral

type Initial =
    { Rep: string
      IPA: string
      Approximation: string
      Explanation: string }

type Final =
    { Rep: string
      IPA: string
      Approximation: string
      Explanation: string }

type Tone =
    { Rep: int
      Description: ToneDescription }

type Pinyin =
    { Initial: Option<Initial>
      Final: Final
      Tone: Tone
      FinalTone: string
    }

// For example
let b = {Rep = "b"; IPA = "p"; Approximation = "s/p/ark"; Explanation = "unaspirated p, as in spark"}

let initials: Initial List = [
    { Rep = "zh"; IPA = "ʈʂ";  Approximation = "nurture"        ; Explanation = "Unaspirated ch. Similar to hatching but retroflex, or marching in American English. Voiced in a toneless syllable." }
    { Rep = "ch"; IPA = "ʈʂʰ"; Approximation = "church "        ; Explanation = "Similar to chin, but retroflex." } 
    { Rep = "sh"; IPA = "ʂ";   Approximation = "shirt "         ; Explanation = "Similar to shoe but retroflex, or marsh in American English." }
    { Rep = "b";  IPA = "p";   Approximation = "s/p/ark"        ; Explanation = "unaspirated p, as in spark" } 
    { Rep = "p";  IPA = "p";   Approximation = "/p/ay "         ; Explanation = "strongly aspirated p, as in pit" } 
    { Rep = "m";  IPA = "m";   Approximation = "may "           ; Explanation = "as in English mummy" } 
    { Rep = "f";  IPA = "f";   Approximation = "fair "          ; Explanation = "as in English fun" } 
    { Rep = "d";  IPA = "t";   Approximation = "stop "          ; Explanation = "unaspirated t, as in stop" } 
    { Rep = "t";  IPA = "t";   Approximation = "take "          ; Explanation = "strongly aspirated t, as in top" }
    { Rep = "n";  IPA = "n";   Approximation = "nay "           ; Explanation = "as in English nit" } 
    { Rep = "l";  IPA = "l";   Approximation = "lay "           ; Explanation = "as in English love" } 
    { Rep = "g";  IPA = "k";   Approximation = "skill "         ; Explanation = "unaspirated k, as in skill" } 
    { Rep = "k";  IPA = "kʰ";  Approximation = "kay "           ; Explanation = "strongly aspirated k, as in kill" } 
    { Rep = "h";  IPA = "h/x"; Approximation = "loch "          ; Explanation = "Varies between hat and Scottish loch." } 
    { Rep = "j";  IPA = "tɕ";  Approximation = "churchyard"     ; Explanation = "Alveo-palatal. No equivalent in English, but similar to an unaspirated \"-chy-\" sound when said quickly. Like q, but unaspirated. Is similar to the English name of the letter G, but curl the tip of the tongue downwards to stick it at the back of the teeth." } 
    { Rep = "q";  IPA = "tɕʰ"; Approximation = "punch yourself" ; Explanation = "Alveo-palatal. No equivalent in English. Like punch yourself, with the lips spread wide as when one says ee. Curl the tip of the tongue downwards to stick it at the back of the teeth and strongly aspirate." } 
    { Rep = "x";  IPA = "ɕ";   Approximation = "push yourself"  ; Explanation = "Alveo-palatal. No equivalent in English. Like -sh y-, with the lips spread as when one says ee and with the tip of the tongue curled downwards and stuck to the back of the teeth." } 
    { Rep = "r";  IPA = "ɻ~ʐ"; Approximation = "ray "           ; Explanation = "No equivalent in English, but similar to a sound between r in reduce and s in measure but with the tongue curled upward against the top of the mouth (i.e. retroflex)." } 
    { Rep = "z";  IPA = "ts";  Approximation = "pizza "         ; Explanation = "unaspirated c, similar to something between suds but voiceless, unless in a toneless syllable." } 
    { Rep = "c";  IPA = "tsʰ"; Approximation = "hats "          ; Explanation = "like the English ts in cats, but strongly aspirated, very similar to the Czech, Polish, Esperanto, and Slovak c." } 
    { Rep = "s";  IPA = "s";   Approximation = "say "           ; Explanation = "as in sun" } 
    { Rep = "w";  IPA = "w";   Approximation = "way "           ; Explanation = "as in water. Before an e or a it is sometimes pronounced like v as in violin.*" } 
    { Rep = "y";  IPA = "j/ɥ"; Approximation = "yes "           ; Explanation = "as in yes. Before a u, pronounced with rounded lips, as if pronouncing German ü.*" } 
]



let finals: Final List = [
  { Rep = "ang"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "an"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ai"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ao"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "iang"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ian"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ia"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "iong"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ie"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ing"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "in"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "iu"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "Üan"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "Üe"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ün"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ü"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "uang"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "uan"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ua"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ui"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "un"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "uo"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "eng"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "en"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ei"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "e"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ou"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "ong"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "i"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "o"; IPA = ""; Approximation = ""; Explanation = "" }
  { Rep = "a"; IPA = ""; Approximation = ""; Explanation = "" }
]

let tones = [
    { Rep = 1; Description = High}
    { Rep = 2; Description = Rising}
    { Rep = 3; Description = Dipping}
    { Rep = 4; Description = Falling}
    { Rep = 0; Description = Neutral}
]

let regexIsMatch (pattern : string) (input : string) =
    Regex.IsMatch(input, pattern)

let regexReplace (pattern : string) (replacement : string) (input : string) =
    Regex.Replace(input, pattern, replacement)

let findTone (tones : Tone List) (str : string ) =
    if (regexIsMatch ".*[āēīōūǖ1].*" str) then { Rep = 1; Description = High}
    elif (regexIsMatch ".*[áéíóúǘ2].*" str) then { Rep = 2; Description = Rising}
    elif (regexIsMatch ".*[ǎěǐǒǔǚ3].*" str) then { Rep = 3; Description = Dipping}
    elif (regexIsMatch ".*[àèìòùǜ4].*" str) then { Rep = 4; Description = Falling}
    else { Rep = 0; Description = Neutral}

let cleanString str =
    str |> regexReplace "[āáǎà]" "a"
    |> regexReplace "[ēéěè]" "e"
    |> regexReplace "[īíǐì]" "i"
    |> regexReplace "[ōóǒò]" "o"
    |> regexReplace "[ūúǔù]" "u"
    |> regexReplace "[ǖǘǚǜ]" "ü"

//let vowels = ["a","e","i","u","ü"]
let aas = ["a";"ā";"á";"ǎ";"à"]
let ees = ["e";"ē";"é";"ě";"è"]
let iis = ["i";"ī";"í";"ǐ";"ì"]
let oos = ["o";"ō";"ó";"ǒ";"ò"]
let uus = ["u";"ū";"ú";"ǔ";"ù"]
let üüs = ["ü";"ǖ";"ǘ";"ǚ";"ǜ"]

let addTone (tone : Tone) (final : string) =
    let n = tone.Rep 
    let r = regexReplace

    if final.Contains("iu") then r "u" uus.[n] final
    elif final.Contains("a") then r "a" aas.[n] final
    elif final.Contains("e") then r "e" ees.[n] final
    elif final.Contains("i") then r "i" iis.[n] final
    elif final.Contains("o") then r "o" oos.[n] final
    elif final.Contains("u") then r "u" uus.[n] final
    elif final.Contains("ü") then r "ü" üüs.[n] final
    else final


let StringToPinyinLists (initials : Initial List) (finals : Final List) (tones : Tone List) (str : string) =
    let cleanStr = cleanString str

    let initialOption = initials |> List.tryFind (fun n -> cleanStr.Contains(n.Rep.ToString()))
    let finalOption = finals |> List.tryFind (fun n -> cleanStr.Contains(n.Rep.ToString()))
    let toneOption = findTone tones str

    match (initialOption, finalOption, toneOption) with
    | (initial, Some final, tone) -> Some({ Initial = initial;
                                            Final = final;
                                            Tone = tone;
                                            FinalTone = addTone tone final.Rep })
    | _ -> None

let StringToPinyin str =
    StringToPinyinLists initials finals tones str
