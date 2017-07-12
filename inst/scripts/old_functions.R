#' Create column specification
#'
#' Return the \code{readr::\link[readr]{cols}} column specification for the Data Package Table Schema column type.
#'
#' @param type Field type.
#' @param format Format specification (e.g. see \code{\link[readr]{parse_datetime}}). If set to "", date times are parsed as ISO8601, and dates and times are parsed using the date and time formats specified in the \code{\link[readr]{locale}}.
#' @export
#' @examples
#' .readr_col("string")
#' .readr_col("number")
#' .readr_col("datetime")
#' .readr_col("boolean")
.readr_col <- function(type, format = "") {
  switch(
    type,
    string = readr::col_character(),
    number = readr::col_number(),
    integer = readr::col_integer(),
    date = readr::col_date(format = format),
    datetime = readr::col_datetime(format = format),
    boolean = readr::col_logical(),
    readr::col_guess()
  )
}

#' Trim list
#'
#' Removes \code{NULL} and empty lists from a list recursively.
#'
#' @param x List.
#' @export
#' @examples
#' x <- list(
#'   NULL,
#'   list(),
#'   list(list(TRUE, list())),
#'   list(NULL, NULL, list()),
#'   1,
#'   list(2, NULL),
#'   df = data.frame(a = 1, b = 2),
#'   integer(),
#'   list(integer())
#' )
#' trim_list(x) %>% str()
#' trim_list(list(NULL))
trim_list <- function(x) {
  .rm_null <- function(x) {
    if (rlang::is_bare_list(x)) {
      x %<>%
        subset(!sapply(., is.null)) %>%
        lapply(.rm_null)
    }
    x
  }
  .is_not_empty <- function(x) {
    if (rlang::is_bare_list(x)) {
      any(sapply(x, .is_not_empty))
    } else {
      length(x) > 0
    }
  }
  .rm_empty <- function(x) {
    if (rlang::is_bare_list(x) && length(x) > 0) {
      x %<>%
        subset(sapply(., .is_not_empty)) %>%
        lapply(.rm_empty)
    }
    x
  }
  x %>%
    .rm_null() %>%
    .rm_empty()
}

