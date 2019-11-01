


.onAttach <- function(...) {

  if (.Platform$OS.type != "windows") {
    packageStartupMessage("cannot use this package: not on Windows")
  } else if (!can_call_vbscript_files()) {
    packageStartupMessage("cannot use this package: cannot execute vbscript ",
                          "files; see ?can_call_vbscript_files")
  }
  invisible(NULL)
}


