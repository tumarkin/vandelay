let command = ./command.dhall

let PageBreakConfig =
        { font_size : Text
    , column_specification: Text
    , table_header: Text
    , number_of_models: Natural
    }
    
in \(pbc: PageBreakConfig) -> [command.latex
''
\bottomrule
\addlinespace
\multicolumn{${Natural/show pbc.number_of_models}}{r}{(\emph{Continued})}
\end{tabular}
\end{center}

\end{table}

\addtocounter{table}{-1}
\begin{table}[ht!]
\caption{Continued}
\${pbc.font_size}

\begin{center}
\medskip
\begin{tabular}{${pbc.column_specification}}

${pbc.table_header}
''
]

