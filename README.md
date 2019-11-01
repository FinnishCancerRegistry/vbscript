# vbscript

Enables easy calling of vbscript files from R.
This package can be used to automatically interact with GUIs in Windows
when there is no other choice.

## Installation

``` r
devtools::install_github("WetRobot/vbscript")
```

## Example

``` r
library(vbscript)

call_vbscript_lines(vbscript_lines_echo("hello world!"))
```

