




normalize_path <- function(path, double_slash = FALSE) {
  assert_is_character_nonNA_atom(path)
  assert_is_logical_nonNA_atom(double_slash)
  path <- normalizePath(path = path, winslash = "\\", mustWork = FALSE)
  path <- gsub("\\{2,}", "\\", path)

  if (double_slash) {
    path <- gsub("\\", "\\\\", path, fixed = TRUE)
  }

  is_dir <- dir.exists(path)
  path[is_dir] <- paste0(path, "\\")

  path

}





#' @title vbscript Files
#' @description Utilities to read, write, and call vbscript files.
#' @name vbscript_file_utils


#' @rdname vbscript_file_utils
#' @export
#' @details
#' - `can_call_vbsript_files`: returns `TRUE` if you can call vbscript files on
#' this system; this function tries calling [call_vbscript_lines] with a very
#' simple command
can_call_vbscript_files <- function() {
  string <- "__%%__VERY_UNLIKELY_STRING_INDEED__%%__"
  stdout <- call_vbscript_lines(vbscript_lines_echo(string))

  string %in% stdout
}

#' @rdname vbscript_file_utils
#' @export
#' @details
#' - `write_vbscript_file`: simply calls [writeLines] after some checks
#' @param x `[character]` (mandatory, no default)
#'
#' - `write_vbscript_file`: lines to write to file
#' - `call_vbscript_lines`: lines to call
#' @param file_path `[character]` (mandatory, no default)
#'
#' path where to write file / read file from
#' @param ...
#'
#' - `write_vbscript_file`: arguments passed to [writeLines],
#'   except `text` and `con`
#' - `read_vbscript_file`: arguments passed to [readLines], expect `con`
write_vbscript_file <- function(x, file_path, ...) {
  assert_file_is_writable(file_path)
  assert_is_character_nonNA_vector(x)
  stopifnot(
    grepl("\\.vbs$", file_path)
  )
  writeLines(text = x, con = file_path, ...)
}

#' @rdname vbscript_file_utils
#' @export
#' @details
#' - `read_vbscript_file`: simply calls [readLines] after some checks
#'   and returns a `vbscript_lines` object (see [as.vbscript_lines])
read_vbscript_file <- function(file_path, ...) {
  assert_file_exists(file_path)
  stopifnot(
    grepl("\\.vbs$", file_path)
  )
  lines <- readLines(con = file_path, ...)
  as.vbscript_lines(lines)
}



#' @rdname vbscript_file_utils
#' @export
#' @details
#' - `call_vbscript_file`: calls the specified file as a vbscript file using
#'   Windows executable `cscript.exe`; output and errors are captured into R
#'   and returned; see [system2]
call_vbscript_file <- function(file_path) {
  assert_file_exists(file_path, "file_path")
  stdout <- system2(
    command = "cscript.exe",
    args = paste0("\"", file_path, "\""),
    stdout = TRUE,
    stderr = TRUE
  )
  stdout
}




#' @rdname vbscript_file_utils
#' @export
#' @details
#' - `call_vbscript_lines`: writes `x` into a temporary file and passes that
#'   file's path to [call_vbscript_file]
call_vbscript_lines <- function(x) {

  tf <- tempfile(fileext = ".vbs")
  writeLines(text = "", con = tf)
  tf <- normalize_path(tf)
  on.exit({
    if (file.exists(tf)) {
      file.remove(tf)
    }
  })
  write_vbscript_file(x, file = tf)
  call_vbscript_file(tf)
}





#' @title `vbscript` Code Generation
#' @description
#' Utilities to generate vbscript code in R.
#' @name vbscript_code_generation_utils
#' @return a `vbscript_lines` object (see [as.vbscript_lines])

#' @rdname vbscript_code_generation_utils
#' @export
#' @details
#' - `vbscript_lines_echo`: simply generates a `Wscript.Echo` call using `x`
#' @param x `[character]` (mandatory, no default)
#'
#' a string to echo in vbscript
vbscript_lines_echo <- function(x) {
  assert_is_character_nonNA_atom(x)
  as.vbscript_lines(paste0('Wscript.Echo("', x,'")'))
}





#' @rdname vbscript_code_generation_utils
#' @export
#' @details
#' - `vbscript_lines_set_focus_to_window`: generates a `FocusShell.AppActivate`
#' call using `window_name`;  if such a window does not exist,
#' this function just fails to set focus to anything
#' (without indicating failure in R or otherwise)
#' @param window_name `[character]` (mandatory, no default)
#'
#' set Windows focus to window by this name; if such a window does not exist,
#' this function just fails to set focus to anything
#' (without indicating failure in R or otherwise)
vbscript_lines_set_focus_to_window <- function(window_name) {
  assert_is_character_nonNA_atom(window_name)

  lines <- c(
    "",
    'Set FocusShell = WScript.CreateObject("WScript.Shell")',
    paste0("FocusShell.AppActivate(\"", window_name, "\")"),
    ""
  )
  as.vbscript_lines(lines)
}




#' @rdname vbscript_code_generation_utils
#' @export
#' @details
#' - `vbscript_lines_execute_keystrokes`: generates `WshShell.SendKey`
#' calls using `keystrokes`
#' @param keystrokes `[character]` (mandatory, no default)
#'
#' one or more keystrokes to execute; specials include `{ENTER}` for Enter,
#' `{TAB}` for Tab, etc.
#' @param init_shell `[logical]` (mandatory, default `TRUE`)
#'
#' keystrokes can only be executed if `WScript.Shell` has been initiated earlier
#' in the script; if this argument is `TRUE`, this initiation is preprended to
#' output of this function
vbscript_lines_execute_keystrokes <- function(keystrokes, init_shell = TRUE) {
  assert_is_character_nonNA_vector(keystrokes)
  assert_is_logical_nonNA_atom(init_shell)
  lines <- paste0("WshShell.SendKeys(\"", keystrokes, "\")")
  if (init_shell) {
    lines <- c(
      'Set WshShell = WScript.CreateObject("WScript.Shell")',
      lines
    )
  }
  as.vbscript_lines(lines)
}











