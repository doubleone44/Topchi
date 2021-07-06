module Home.Types

open Pinyin

type Model =
    | Pinyin of Pinyin
    | Fail of string

type Msg =
    | ChangeStr of string
