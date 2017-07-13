# ---- Fields ----

#' Set field metadata
#'
#' New properties are added and existing properties are replaced (or deleted if \code{NULL}). If set, properties override the defaults returned by \code{\link{get_field}}.
#'
#' @export
#' @param .data Object to modify (typically an atomic vector).
#' @inheritParams field
#' @family field functions
#' @examples
#' x <- set_field(
#'  Sys.Date(),
#'  name = "date_created",
#'  type = "date",
#'  format = "%Y-%m-%d"
#' )
#' get_field(x)
set_field <- function(.data, name, type, format, title, description, rdfType, constraints, unit, ...) {
  meta <- get_called_args(...)
  .data %>%
    modify_attr(which = "dpkg_field", value = meta) %>%
    sticky::sticky()
}
#' @rdname set_field
#' @param value (named list) Field metadata (typically a call to \code{\link{field}}).
#' @export
#' @examples
#' x <- Sys.Date()
#' set_field(x) <- field(name = "date_created", type = "date", format = "%Y-%m-%d")
#' get_field(x)
`set_field<-` <- function(.data, value) {
  .data %>%
    modify_attr(which = "dpkg_field", value = value) %>%
    sticky::sticky()
}

#' Set field metadata by name
#'
#' @export
#' @param l (list) List of objects to modify.
#' @param ... (name = value) Field metadata (named list or call to \link{field}) to assign to the object of the given name.
#' @family field functions
#' @examples
#' df <- set_fields(
#'   data.frame(id = 1L, value = 1.1),
#'   value = field(title = "Value"),
#'   id = field(title = "Unique identifier", constraints = constraints(unique = TRUE))
#' )
#' get_fields(df)
set_fields <- function(l, ...) {
  set_by_name(l, set_field, ...)
}

#' Get field metadata
#'
#' Missing properties are returned filled with their default values (see Details).
#'
#' @details
#' Unless set explicity, the following defaults are returned:
#' \itemize{
#'   \item name: The name of the object in a list.
#'   \item type: The type corresponding to the object class (or "string" if not supported).
#'   \item format: The default format for that type.
#'   \item unit: Units set by \code{\link[units]{units}} deparsed to product power form (if available).
#' }
#'
#' @export
#' @param x Object.
#' @param name (character) Field name to return if not set explicitly.
#' @family field functions
#' @examples
#' get_field(1)
#' get_field(1, "id")
#' x <- set_field(1, type = "logical")
#' get_field(x)
#' get_field(Sys.time())
get_field <- function(x, name = NULL) {
  meta <- attr(x, "dpkg_field", exact = TRUE)
  if (is.null(meta$name)) {
    meta$name <- name
  }
  if (identical(meta$name, "")) {
    meta$name <- NULL
  }
  if (is.null(meta$type)) {
    meta$type <- get_field_type(x)
  }
  if (is.null(meta$format)) {
    meta$format <- get_field_format_default(meta$type)
  }
  if (inherits(x, "units") && "units" %in% rownames(utils::installed.packages())) {
    meta$unit <- units::as_cf(x)
  }
  positions <- match(names(meta), names(formals(field)))
  meta[order(positions)]
}
#' @rdname get_field
#' @export
#' @param l (list) List of objects.
#' @examples
#' get_fields(data.frame(id = 1L, value = 1.1))
get_fields <- function(l) {
  lapply(seq_along(l), function(i) {
    get_field(l[[i]], name = names(l)[i])
  })
}

#' Get field type from object class
#'
#' @param x Object.
get_field_type <- function(x) {
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXt")) return("datetime")
  if (is.character(x)) return("string")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("number")
  if (is.logical(x)) return("boolean")
  warning(paste("Object class not supported:", toString(class(x)), "(using string)"))
  return("string")
}

#' Get default format by field type
#'
#' @param type (character) Field type.
get_field_format_default <- function(type) {
  switch(
    type,
    date = "%Y-%m-%d",
    datetime = "%Y-%m-%dT%H:%M:%SZ",
    NULL
  )
}

# ---- Resources ----

#' Set resource metadata
#'
#' New properties are added and existing properties are replaced (or deleted if \code{NULL}). If set, properties override the defaults returned by \code{\link{get_resource}}.
#'
#' @export
#' @param .data Object to modify (typically a data.frame).
#' @inheritParams resource
#' @family resource functions
#' @examples
#' x <- set_resource(
#'   data.frame(id = 1L, value = 1.1),
#'   title = "Example data",
#'   path = "data/example.csv"
#' )
#' get_resource(x)
set_resource <- function(.data, name, path, profile, title, description, format, mediatype, encoding, schema, bytes, hash, sources, licenses, ...) {
  meta <- get_called_args(...)
  .data %>%
    modify_attr(which = "dpkg_resource", value = meta) %>%
    sticky::sticky()
}
#' @rdname set_resource
#' @param value (named list) Resource metadata (typically a call to \code{\link{resource}}).
#' @export
#' @examples
#' x <- data.frame(id = 1L, value = 1.1)
#' set_resource(x) <- resource(title = "Example data", path = "data/example.csv")
#' get_resource(x)
`set_resource<-` <- function(.data, value) {
  .data %>%
    modify_attr(which = "dpkg_resource", value = value) %>%
    sticky::sticky()
}

#' Set resource metadata by name
#'
#' @export
#' @param l (list) List of objects to modify.
#' @param ... (name = value) Resource metadata (named list or call to \link{resource}) to assign to the object of the given name.
#' @family resource functions
#' @examples
#' l <- set_resources(
#'   list(a = data.frame(), b = data.frame()),
#'   a = resource(path = "data/a.csv"),
#'   b = resource(path = "data/b.csv")
#' )
#' get_resources(l)
set_resources <- function(l, ...) {
  set_by_name(l, set_resource, ...)
}

#' Get resource metadata
#'
#' Missing properties are returned filled with their default values (see Details). If \code{inline_data = TRUE} and property \code{path} is \code{NULL}, object data is returned in the \code{data} property either unchanged (if \code{format} is missing or "json") or formatted as a string (provided the format is supported).
#'
#' @details
#' Unless set explicity, the following defaults are returned:
#' \itemize{
#'   \item name: The name of the object in a list.
#'   \item schema$fields: Field metadata from the elements of the object, via \code{\link{get_field}}.
#' }
#'
#' @export
#' @param x Object.
#' @param name (character) Resource name to return if not set explicitly.
#' @param inline_data (logical) Whether to include the contents of \code{x} as inline data (\code{$data}).
#' @family resource functions
#' @examples
#' x <- data.frame(id = 1L, value = 1.1)
#' get_resource(x)
#' get_resource(x, "data")
get_resource <- function(x, name = NULL, inline_data = TRUE) {
  meta <- attr(x, "dpkg_resource", exact = TRUE)
  if (is.null(meta$name)) {
    meta$name <- name
  }
  if (identical(meta$name, "")) {
    meta$name <- NULL
  }
  if (is.null(meta$schema$fields)) {
    meta$schema$fields <- get_fields(x)
  }
  if (length(meta$schema$fields) > 0) {
    field_names <- lapply(meta$schema$fields, "[[", "name")
    missing_name <- sapply(field_names, is.null)
    if (any(missing_name)) {
      stop(paste0("Resource has fields with no name at positions: ", toString(which(missing_name))))
    }
  }
  if (inline_data && is.null(meta$path)) {
    format <- parse_resource_format(meta)
    if (format == "json") {
      # JSON object
      if (is.null(meta$schema$fields)) {
        meta$data <- x
      } else {
        meta$data <- x %>% deparse_fields(meta = meta$schema$fields)
      }
    } else {
      # Formatted string
      meta$data <- write_resource(x)
    }
  }
  positions <- match(names(meta), names(formals(resource)))
  meta[order(positions)]
}
#' @rdname get_resource
#' @export
#' @param l (list) List of objects.
#' @examples
#' l <- set_resources(
#'   list(a = data.frame(), b = data.frame(x = 1)),
#'   a = resource(path = "data/a.csv"),
#'   b = resource(path = "data/b.csv")
#' )
#' get_resources(l)
get_resources <- function(l, inline_data = FALSE) {
  lapply(seq_along(l), function(i) {
    get_resource(l[[i]], name = names(l)[i], inline_data = inline_data)
  })
}

# ---- Packages ----

#' Set package metadata
#'
#' New properties are added and existing properties are replaced (or deleted if \code{NULL}). If set, properties override the defaults returned by \code{\link{get_package}}.
#'
#' @export
#' @param .data Object to modify (typically a list of data.frames).
#' @inheritParams package
#' @family package functions
#' @examples
#' x <- set_package(
#'   list(data = data.frame(id = 1L, value = 1.1)),
#'   title = "Example data package"
#' )
#' get_package(x)
set_package <- function(.data, name, title, description, homepage, id, profile, version, created, sources, contributors, licenses, keywords, image, resources, ...) {
  meta <- get_called_args(...)
  .data %>%
    modify_attr(which = "dpkg_package", value = meta) %>%
    sticky::sticky()
}
#' @rdname set_package
#' @param value (named list) Package metadata (typically a call to \code{\link{package}}).
#' @export
#' @examples
#' x <- list(data = data.frame(id = 1L, value = 1.1))
#' set_package(x) <- package(title = "Example data package")
#' get_package(x, inline_data = FALSE)
`set_package<-` <- function(.data, value) {
  .data %>%
    modify_attr(which = "dpkg_package", value = value) %>%
    sticky::sticky()
}

#' Get package metadata
#'
#' Missing properties are returned filled with their default values (see Details). An error is returned if the resources property is empty.
#'
#' @details
#' Unless set explicity, the following defaults are returned:
#' \itemize{
#'   \item resources: Resource metadata from the elements of the object, via \code{\link{get_resource}}.
#' }
#'
#' @export
#' @param x Object.
#' @param name (character) Package name to return if not set explicitly.
#' @inheritParams get_resource
#' @family resource functions
#' @examples
#' x <- list(data = data.frame(id = 1L, value = 1.1))
#' get_package(x, inline_data = FALSE)
get_package <- function(x, name = NULL, inline_data = TRUE) {
  meta <- attr(x, "dpkg_package", exact = TRUE)
  if (is.null(meta$name)) {
    meta$name <- name
  }
  if (is.null(meta$resources)) {
    meta$resources <- get_resources(x, inline_data = inline_data)
  }
  if (length(meta$resources) == 0) {
    stop("Package has no resources.")
  } else {
    resource_names <- lapply(meta$resources, "[[", "name")
    missing_name <- sapply(resource_names, is.null)
    if (any(missing_name)) {
      stop(paste0("Package has resources with no name at positions: ", toString(which(missing_name))))
    }
  }
  positions <- match(names(meta), names(formals(package)))
  meta[order(positions)]
}
