# ---- Field ----

#' Deparse field
#'
#' @details
#' NOTES:
#' \itemize{
#'   \item Ignores missingValues (leaves to write_* functions)
#'   \item In some cases, leaves final coercion to write_* functions
#' }
#'
#' @param x (atomic) Object to deparse.
#' @param meta (list) Field metadata.
#' @examples
#' deparse_field(Sys.Date())
#' deparse_field(Sys.time())
#' deparse_field("string")
#' deparse_field(1)
#' @export
deparse_field <- function(x, meta = get_field(x)) {
  if (!is.character(x)) {
    deparser <- paste0("deparse_", meta$type) %>% get()
    x %<>% deparser()
  }
  x %>% `attributes<-`(NULL)
}

#' @rdname deparse_field
deparse_string <- function(x) {
  stopifnot(is.character(x))
  x
}

#' @rdname deparse_field
deparse_number <- function(x) {
  stopifnot(is.numeric(x) || is.integer(x))
  x %<>% `attributes<-`(NULL)
  meta <- get_field(x)
  decimalChar <- coalesce(meta$decimalChar, ".")
  groupChar <- coalesce(meta$groupChar, "")
  if (decimalChar != "." || groupChar != "") {
    x %>% prettyNum(decimal.mark = decimalChar, big.mark = groupChar, big.interval = 3L)
  } else {
    x # leave for write_* functions
  }
}

#' @rdname deparse_field
deparse_integer <- function(x) {
  stopifnot(is.integer(x))
  x # leave for write_* functions
}

#' @rdname deparse_field
deparse_boolean <- function(x) {
  stopifnot(is.logical(x))
  meta <- get_field(x)
  if (is.null(meta$trueValues) && is.null(meta$falseValues)) {
    x # leave for write_* functions
  } else {
    character(length(x)) %>%
      replace(x, meta$trueValues[1]) %>%
      replace(!x, meta$falseValues[1])
  }
}

#' @rdname deparse_field
deparse_date <- function(x) {
  stopifnot(inherits(x, "Date"))
  meta <- get_field(x) # format
  x %>% format(format = meta$format, tz = "UTC")
}

#' @rdname deparse_field
deparse_datetime <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  meta <- get_field(x) # format
  x %>% format(format = meta$format, tz = "UTC")
}

# ---- Field list ----

#' Deparse list of fields
#' @param l (list of atomic vectors)
#' @param meta (list of lists) Field metadata
#' @export
deparse_fields <- function(l, meta = get_fields(l)) {
  for (i in seq_along(l)) {
    l[[i]] %<>% deparse_field(meta = meta[[i]])
  }
  l
}
