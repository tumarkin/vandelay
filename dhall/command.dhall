let surround = ./surround.dhall
let text = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v19.0.0/Prelude/Text/package.dhall

let bracket = \(t : Text) -> "{" ++ t ++ "}"

let FormatSpec =
    { index : Natural
    , surround: Optional surround.Surround
    , format : Text
    , scale : Double
    , reformat_zero : Bool
    , empty : Optional Text
    }

let RowSpec =
    { name : Text
    , code : List Text
    , format_spec : FormatSpec
    }

let Command =  <Latex : Text | Row : RowSpec | SetActiveModels : List {file : Text, column : Text}>

let Target = <Latex | Typst>

let latex            = \(t : Text) -> Command.Latex t
let latex_ln         = \(t : Text) -> Command.Latex (t ++ " \\\\")
let latex_columns_ln = \(fields : List Text) -> latex_ln (text.concatMapSep " & " Text bracket fields)
let row              = \(format_spec : FormatSpec) -> \(name : Text) -> \(code : List Text) -> Command.Row {name, code, format_spec}
let set_active_models = \(models : List {file : Text, column : Text}) -> Command.SetActiveModels models
   
in { Command
   , FormatSpec
   , latex
   , latex_ln
   , latex_columns_ln
   , row
   , set_active_models
   , Target
   }
