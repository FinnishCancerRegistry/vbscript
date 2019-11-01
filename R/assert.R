


assert_has_class <- function(x, x.nm = NULL, required.class) {
  stopifnot(
    length(required.class) == 1,
    is.character(required.class)
  )
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  if (!inherits(x, required.class)) {
    stop("expected ", deparse(x.nm), " to have class ", deparse(required.class),
         "; instead it had class(es) ", deparse(class(x)))
  }
  invisible(NULL)
}
assert_is_character <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_has_class(x = x, x.nm = x.nm, required.class = "character")
}
assert_is_atom <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  if (length(x) != 1) {
    stop("expected ", deparse(x.nm), " to have length 1; instead it had length",
         " ", length(x))
  }
  invisible(NULL)
}
vector_modes <- function() {
  # ?is.vector
  c("logical", "integer", "numeric", "complex", "character", "raw")
}
assert_is_vector <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  if (!inherits(x, vector_modes())) {
    stop("expected ", deparse(x.nm), " to be a vector (to have one of the ",
         "following classes: ", deparse(vector_modes()), "); instead it had ",
         "class(es) ", deparse(class(x)))
  }
  invisible(NULL)
}
assert_is_nonNA_atom <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_atom(x, x.nm)
  if (is.na(x)) {
    stop("expected ", deparse(x.nm), " to not be NA")
  }
  invisible(NULL)
}
assert_is_nonNA_vector <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_vector(x, x.nm)
  n_na <- anyNA(x)
  if (n_na > 0) {
    stop("expected ", deparse(x.nm), " to not have any NA elements; it had ",
         n_na, " NA elements")
  }
  invisible(NULL)
}
assert_is_character_nonNA_atom <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_character(x, x.nm)
  assert_is_nonNA_atom(x, x.nm)
  invisible(NULL)
}
assert_is_character_nonNA_vector <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_character(x, x.nm)
  assert_is_nonNA_vector(x, x.nm)
  invisible(NULL)
}

assert_is_logical <-  function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_has_class(x = x, x.nm = x.nm, required.class = "logical")
}
assert_is_logical_nonNA_atom <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_logical(x, x.nm)
  assert_is_nonNA_atom(x, x.nm)
  invisible(NULL)
}
assert_is_logical_nonNA_vector <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_logical(x, x.nm)
  assert_is_nonNA_vector(x, x.nm)
  invisible(NULL)
}


assert_file_exists <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_character_nonNA_atom(x, x.nm)
  if (!file.exists(x)) {
    stop("expected ", deparse(x), " to be path to an existing file")
  }
  invisible(NULL)
}

assert_dir_exists <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_character_nonNA_atom(x, x.nm)
  if (!dir.exists(x)) {
    stop("expected ", deparse(x), " to be path to an existing directory")
  }
  invisible(NULL)
}





dir_is_readable <- function(dir.path) {
  assert_dir_exists(dir.path)
  unname(file.access(dir.path, mode = 4L)) == 0L
}

dir_is_writable <- function(dir.path) {
  assert_dir_exists(dir.path)
  unname(file.access(dir.path, mode = 2L)) == 0L
}

assert_dir_is_writable <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- paste0(deparse(substitute(x)), collapse = "")
  }
  assert_is_character_nonNA_atom(x, x.nm)
  assert_dir_exists(x = x, x.nm = x.nm)
  if (!dir_is_writable(x)) {
    stop("Directory ", deparse(x), " exists but is not writable; ensure ",
         "you have writing permissions there.")
  }
  invisible(NULL)
}
assert_file_is_writable <- function(x, x.nm = NULL) {
  if (is.null(x.nm)) {
    x.nm <- deparse(substitute(x))
  }
  assert_is_character_nonNA_atom(x, x.nm)
  implied_dir <- dirname(x)
  assert_dir_is_writable(implied_dir, x.nm)
}


