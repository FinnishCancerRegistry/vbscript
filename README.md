# vbscript

<!-- badges: start -->
[![R-CMD-check](https://github.com/FinnishCancerRegistry/vbscript/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/vbscript/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Enables easy calling of vbscript files from R.
This package can be used to automatically interact with GUIs in Windows
when there is no other choice.

## Installation

``` r
devtools::install_github("WetRobot/vbscript")
```

## Examples

Print something in vbscript

``` r
library(vbscript)

call_vbscript_lines(vbscript_lines_echo("hello world!"))
```

Execute some keystrokes in notepad

``` r
library(vbscript)

shell.exec("notepad")
write_to_notepad <- c(
  vbscript_lines_set_focus_to_window("Notepad"), 
  vbscript_lines_execute_keystrokes(c("h", "e", "l", "l", "o")), 
  vbscript_lines_set_focus_to_window("Notepad"), 
  vbscript_lines_execute_keystrokes(c("{ENTER}", "w","o","r","l", "d", "!"))
)
call_vbscript_lines(write_to_notepad)
```

