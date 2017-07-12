# ---- JSON ----

#' Read JSON
#'
#' @param txt (character) JSON string or path.
#' @param tabular (logical) Whether to read \code{txt} as tabular data.
#' @family formats
#' @references \url{https://specs.frictionlessdata.io/tabular-data-resource/#json-tabular-data}
read_json <- function(txt, tabular = FALSE) {
  x <- txt %>% jsonlite::fromJSON(simplifyVector = FALSE)
  if (tabular) {
    tabulate_json(x)
  } else {
    x
  }
}

tabulate_json <- function(x) {
  # Replace NULL with NA
  x %<>% lapply(function(xi) {
    xi %>% replace(sapply(., is.null), NA)
  })
  if (is.list(x) && !all(sapply(x, is.list))) {
    # Single row
    x %<>% list()
  }
  if (length(names(x[[1]])) > 0) {
    # Object array
    x %<>%
      data.table::rbindlist(use.names = TRUE)
  } else {
    # Row array
    col_names <- unlist(x[[1]])
    x <- x[-1] %>%
      data.table::rbindlist(use.names = FALSE) %>%
      set_names(col_names)
  }
  x %>%
    as.data.frame()
}

#' Write JSON
#'
#' @param x Object to write.
#' @family formats
#' @param file (character) Path to file on disk. If \code{NULL}, a JSON string is returned.
write_json <- function(x, file = NULL) {
  txt <- x %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  if (is.null(file)) {
    txt
  } else {
    writeLines(txt, file)
  }
}

# ---- CSV ----

#' Read CSV
#'
#' TODO: Distinguish between empty and empty quoted string.
#'
#' @param txt (character) CSV string or path.
#' @param na (character) Strings interpreted as \code{NA}.
#' @param colClasses (character) Classes to assume for the columns.
#' @family formats
read_csv <- function(txt, na = NULL, colClasses = NULL) {
  txt %>%
    data.table::fread(
      sep = ",",
      na.strings = na,
      quote = "\"",
      encoding = "UTF-8",
      colClasses = colClasses,
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
}

#' Write CSV
#'
#' @param x Object to write.
#' @param file (character) Path to file on disk. If \code{NULL}, a CSV string is returned.
#' @param na (character) String to use for missing values.
#' @family formats
write_csv <- function(x, file = NULL, na = "") {
  txt <- R.utils::captureOutput(
    x %>%
      data.table::fwrite(
        file = coalesce(file, ""),
        sep = ",",
        na = na,
        quote = "auto"
      ),
    collapse = "\n"
  )
  if (is.null(file)) {
    txt
  }
}
