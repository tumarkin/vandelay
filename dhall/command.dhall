let text = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v19.0.0/Prelude/Text/package.dhall

let bracket = \(t : Text) -> "{" ++ t ++ "}"

let Surround = { before: Text, after: Text }

let FormatSpec =
    { index : Natural
    -- , surround : Optional Surround
    , format : Text
    , scale : Double
    , empty : Optional Text
    }

let RowSpec =
    { name : Text
    , code : List Text
    , format_spec : FormatSpec
    }

let Command =  <Latex : Text | Row : RowSpec >

let latex_ln         = \(t : Text) -> Command.Latex (t ++ " \\")
   
in { Command 
   , latex            = \(t : Text) -> Command.Latex t
   , latex_ln
   , latex_columns_ln = \(fields : List Text) -> latex_ln (text.concatMapSep " & " Text bracket fields)
   , row = \(fs : FormatSpec) -> \(name : Text) -> \(code : List Text) -> Command.Row {name, code, format_spec = fs}
   }

