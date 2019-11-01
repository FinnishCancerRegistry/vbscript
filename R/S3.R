




#' @title `vbscript_lines`
#' @description
#' Coercion to class `vbscript_lines`
#' @param x `[character]` (mandatory, no default)
#'
#' character string vector to coerce to `vbscript_lines`
#' @export
as.vbscript_lines <- function(x) {
  UseMethod("as.vbscript_lines")
}

#' @describeIn as.vbscript_lines coerces character string vector to class
#' `vbscript_lines``
#' @export
as.vbscript_lines.character <- function(x) {
  assert_is_character_nonNA_vector(x)
  y <- as.character(x)
  class(y) <- c("vbscript_lines" , "character")
  y
}

#' @title Print `vbscript_lines`
#' @description
#' Print method for `vbscript_lines` objects
#' @param x `[vbscript_lines]` (mandatory, no default)
#'
#' a `vbscript_lines` object
#' @param max.print `[integer, numeric]` (mandatory, default 50)
#'
#' maximum number of lines allowed to be printed; if `x` has more elements
#' than this, only the fist 10 and last 10 elements are shown in print
#' @export
print.vbscript_lines <- function(x, max.print = 50) {
  n_lines <- length(x)
  stopifnot(
    length(max.print) == 1,
    max.print %% 1 == 0,
    max.print > 0
  )

  max.print <- min(max.print, n_lines)

  printable <- rep(TRUE, n_lines)

  if (n_lines > max.print) {
    first_10 <- 1:10
    last_10 <- seq(n_lines, n_lines-9, -1)
    printable[-c(first_10, last_10)] <- FALSE
  }

  cat("--- vbscript_lines vector with", n_lines, "lines ---\n")
  row_num <- which(printable)
  row_num <- formatC(x = row_num, digits = nchar(n_lines), flag = " ")

  if (n_lines > max.print) {
    cat(paste0(row_num[1:10], ": ", x[1:10]), sep = "\n")
    n_hidden_lines <- n_lines-20L
    cat("---", n_hidden_lines, "lines not shown ---\n")
    cat(paste0(row_num[11:20], ": ", x[11:20]), sep = "\n")
  } else {
    cat(paste0(row_num, ": ", x), sep = "\n")
  }
  cat("--- vbscript_lines vector end ---\n")

  invisible(NULL)
}
#' @export
`[.vbscript_lines` <- function(x, ...) {
  y <- NextMethod()
  as.vbscript_lines(y)
}
#' @export
c.vbscript_lines <- function(...) {
  y <- NextMethod()
  as.vbscript_lines(y)
}






