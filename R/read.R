# ---- Package (as data objects) ----

#' Read data package
#'
#' @details
#' NOTES:
#' \itemize{
#'   \item Only supports json and csv formats.
#' }
#'
#' @export
#' @param path (character) Package directory or URL.
#' @param resources (character, numeric) Names or indices of resources to read (or all if \code{NULL}).
#' @references \url{https://specs.frictionlessdata.io/data-resource/}
#' @family package readers
read_package <- function(path = getwd(), resources = NULL) {
  meta <- read_meta(path = path)
  data <- read_data(path, resources = resources, meta = meta, drop = FALSE)
  meta$resources <- NULL
  do.call(set_package, c(list(data), meta))
}
#' @export
#' @rdname read_package
#' @param repo (character) GitHub repository address (see \code{\link{github_raw_url}}).
#' @param ... Additional arguments passed to \code{\link{read_package}}.
read_package_github <- function(repo, ...) {
  repo %>%
    github_raw_url() %>%
    read_package(...)
}

# ---- Metadata ----

#' Read package metadata
#'
#' @inheritParams read_package
#' @family package readers
read_meta <- function(path = getwd()) {
  # Read datapackage.json
  meta <- path %>%
    file.path("datapackage.json") %>%
    read_json()
  # Read external json schemas
  # https://specs.frictionlessdata.io/data-resource/#resource-schemas
  for (i in seq_along(meta$resources)) {
    schema <- meta$resources[[i]]$schema
    if (is.character(schema)) {
      if (!is_url(schema)) {
        schema %<>% file.path(path, .)
      }
      meta$resources[[i]]$schema <- read_json(schema)
    }
  }
  meta
}
#' @rdname read_meta
#' @inheritParams read_package
read_meta_github <- function(repo) {
  repo %>%
    github_raw_url() %>%
    read_meta()
}

# ---- Data ----

#' Read package data
#'
#' @inheritParams read_package
#' @param meta (list) Package metadata.
#' @param drop (logical) Whether to unlist a single resource.
#' @references \url{https://specs.frictionlessdata.io/data-resource/}
#' @family package readers
read_data <- function(path = getwd(), resources = NULL, meta = read_meta(path), drop = FALSE) {
  rnames <- meta$resources %>% sapply("[[", "name")
  # Select resources to read
  if (is.null(resources)) {
    selected <- seq_along(meta$resources)
  } else if (is.character(resources)) {
    selected <- rnames %in% resources
  } else {
    selected <- as.integer(resources)
  }
  # Read resource data
  data <- meta$resources[selected] %>%
    lapply(parse_resource, path = path) %>%
    set_names(rnames[selected])
  if (drop && length(selected) == 1) {
    data[[1]]
  } else {
    data
  }
}
#' @rdname read_data
#' @inheritParams read_package_github
#' @param ... Additional arguments passed to \code{\link{read_data}}.
read_data_github <- function(repo, ...) {
  repo %>%
    github_raw_url() %>%
    read_data(...)
}
