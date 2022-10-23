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
Full detailed documentation is forthcoming. For now, please refer to the example.

# Installation

*vandelay* is written in Haskell. To compile from source, 

* Install Stack: http://haskellstack.org
* Clone this repository. 
* Use the command `stack install` from the project root directory.

# Version history

* 0.1.2.0: [Dhall](https://dhall-lang.org) templating. Greatly improves
           templating flexibility and reduces redundancy.

* 0.1.1.0: All Vandelay templates are processed to a corresponding file with
           the terminal extension changed to 'tex'. The output directory is
           customizable from the command line. Removed destination tex file 
           option from template as this was an unnecessary option.

# To Do

* Improve user messages when a template is misspecified
* Output to other formats (e.g. DOCX)

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
* **DONE** 0.00000 is changed to <0.00001 (for arbitrary number of zeros)
* **DONE** Warn user when a name for a substitution embeds another, but allow for substitutions in substitutions.
           (Unnecessary with Dhall templating)


# Contributing
To do 
