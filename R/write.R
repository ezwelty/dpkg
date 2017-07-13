# ----- Package -----

#' Write package
#'
#' Writes package data and metadata to disk using the following rules for each resource:
#' \itemize{
#'   \item format: If missing, \code{path} file extensions and \code{mediatype} are checked when writing. Only csv and json are supported.
#'   \item path: Only single local paths are supported, and if not set, data is saved in the metadata as an inline JSON object or a formatted string.
#'   \item encoding: Only the default "UTF-8" is supported.
#' }
#'
#' @param x Object.
#' @param name (character) Package name if not set explicitly.
#' @param path (character) Package directory.
#' @inheritParams get_resource
#' @family package writers
#' @export
write_package <- function(x, name = NULL, path = getwd(), inline_data = TRUE) {
  write_meta(x, name = name, path = path, inline_data = inline_data)
  write_data(x, path = path)
}

# ---- Metadata ----

#' Write package metadata
#'
#' @inheritParams write_package
#' @family package writers
write_meta <- function(x, name = NULL, path = getwd(), inline_data = TRUE) {
  meta <- get_package(x, name = name, inline_data = inline_data)
  if (is.null(path) || path == "") {
    file <- NULL
  } else {
    file <- file.path(path, "datapackage.json")
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  }
  write_json(meta, file = file)
}

# ---- Data ----

#' Write package data
#'
#' @inheritParams write_package
#' @family package writers
write_data <- function(x, path = getwd()) {
  stopifnot(is_list_not_df(x))
  for (i in seq_along(x)) {
    meta <- get_resource(x[[i]], inline_data = FALSE)
    if (is.null(meta$path)) {
      next
    }
    write_resource(x[[i]], path = path)
  }
}

#' Write resource data
#'
#' @param x Object.
#' @param path (character) Package directory.
#' @family package writers
write_resource <- function(x, path = NULL) {
  meta <- get_resource(x, inline_data = FALSE)
  x %<>% deparse_fields()
  if (is.null(path)) {
    file <- NULL
  } else {
    stopifnot(!is.null(meta$path))
    file <- file.path(path, meta$path)
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  }
  format <- parse_resource_format(meta)
  switch(
    format,
    csv = write_csv(x, file = file),
    json = write_json(x, file = file)
  )
}
