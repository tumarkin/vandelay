# vandelay
A LaTeX table importer/exporter


# Overview




**Benefits**

* Easily create multiple tables from files containing the results from estimating several specifications. 
* Eliminate the need to re-run your estimation should you decide to reformat your tables.
* Save time when revising your results with automated table generation. 

**Features**

* Join different files of estimation results as necessary.
* Reorder rows and columns as needed.
* Combine coefficients should your software package decide to give a coefficient different names in different specifications.
* Scale and format coefficients on the fly.
* Show or hide key statistics (such as *t*-statistic) as necessary.

# Example
To do

# Documentation
Full detailed documentation is forthcoming. For now, please refer to the  example.

# Installation
You may download a precompiled binary from the /bin directory or build from source.

**Precompiled binary**

A precompiled binary for Mac OS X Mavericks is currently the only one available. Download it from the /bin directory and place it in your user path. Please email me if this does not work as the binary may require additional library files.

**From source**

*vandelay* is written in Haskell. To compile from source, 

* Install Haskell. The Haskell Platform is recommended: <https://www.haskell.org/platform/>. 
* Clone this repository. 
* Use the command `cabal install vandelay.cabal` from the project root directory.

# To Do

* Improve user messages when a template is misspecified
* Output to other formats (e.g. DOCX)
* Warn user when a name for a substitution embeds another, but allow for substitutions in substitutions.

**Completed**

* **DONE** Standardize error messages
* **DONE** Arbitrary scaling of numbers and formatting of missing data.
* **DONE** Support scientific notation in source files.
* **DONE** *vandelay make* subcommand support glob patterns
* **DONE** *vandelay init* subcommand support glob patterns
* **DONE** *vandelay init* supports multiple files with appropriate listing of variable names
* **DONE** Support merging multiple estimation results files 
* **DONE** Allow for comments in the template file
* **DONE** Allow for missing data in variable lists (useful when multiple estimation results files are joined)
* **DONE** Allow data commands to end with a semicolon



# Contributing
To do 
