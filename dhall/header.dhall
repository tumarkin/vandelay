let command = ./command.dhall

let HeaderConfig =
        { caption : Text
    , label : Text
    , header_size : Text
    , header_text : Text
    , column_specification : Text
    , table_header : Text
    , font_size : Text
    }
    
in \(hc: HeaderConfig) ->
[command.latex ''
\begin{table}
\caption{${hc.caption}}
\label{${hc.label}}

\${hc.header_size}
${hc.header_text}

\${hc.font_size}

\begin{center}

\begin{tabular}{${hc.column_specification}}

${hc.table_header}
''
]

     
