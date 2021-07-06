module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Types

let root (model : Model) dispatch =
  div
    [ ]
    [ p
        [ ClassName "control" ]
        [ input
            [ ClassName "input"
              Type "text"
              Placeholder "Type your name"
              DefaultValue ""
              AutoFocus true
              OnChange (fun ev -> !!ev.target?value |> ChangeStr |> dispatch ) ] ]
      br [ ]

      match model with
      | Pinyin pinyin ->
        div
          []
          [
            span [ Style [ Color "#7700AA" ] ] [
                str (match pinyin.Initial with
                     | Some initial -> initial.Rep
                     | None -> "'"
                )   
            ]     

            span [ Style [ Color "#AA0000" ] ] [ str pinyin.FinalTone ]

            br [ ]

            span [ Style [ Color "#000000" ] ] [ str (pinyin.Tone.Description.ToString()) ]

          ]
      | Fail failString -> span [] [ str failString ]

    ]
