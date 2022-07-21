let Surround = { before: Text, after: Text }

let bare         = None Surround
let parentheses  = Some { before = "(", after = ")" }
let curly_braces = Some { before = "{", after = "}" }

in { Surround
   , bare
   , parentheses
   , curly_braces
   }
