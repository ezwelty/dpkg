# ---- Field ----

#' Parse field
#'
#' @details
#' TODO:
#' \itemize{
#'   \item string: Support format property
#'   \item number: Support currency property
#'   \item Validate field against constraints
#' }
#'
#' @export
#' @param x (atomic) Object to parse.
#' @param meta (named list) Field metadata.
#' @references \url{http://specs.frictionlessdata.io/table-schema/#field-descriptors}
#' @family field parsers
parse_field <- function(x, meta = NULL) {
  meta$type %<>% coalesce("string")
  parser <- tryCatch(
    paste0("parse_", meta$type) %>% get(),
    error = function (e) {
      warning(paste("Unsupported field type:", meta$type, "(defaulting to string)"))
      parse_string
    }
  )
  is_parse_arg <- intersect(names(formals(parser)), names(meta))
  x %>%
    {do.call(parser, c(list(.), meta[is_parse_arg]))} %>%
    {do.call(set_field, c(list(.), meta))}
}

#' @rdname parse_field
parse_string <- function(x) {
  if (is.character(x)) {
    x
  } else {
    as.character(x)
  }
}

#' @rdname parse_field
#' @param decimalChar (character) Symbol used to indicate the decimal place.
#' @param groupChar (character) Symbol used to chunk larger numbers.
#' @param unit (character) Unit of measure in product power form (see \code{\link[units]{parse_unit}}).
parse_number <- function(x, decimalChar = ".", groupChar = NULL, unit = NULL) {
  if (!is.numeric(x)) {
    is_percentage <- grepl("%$", x)
    x[is_percentage] %<>% gsub("%{1}$", "", .)
    if (decimalChar != ".") {
      stopifnot(
        is.character(decimalChar),
        length(decimalChar) == 1,
        nchar(decimalChar) == 1
      )
      x %<>% gsub(decimalChar, ".", .)
    }
    if (!is.null(groupChar)) {
      stopifnot(
        is.character(groupChar),
        length(groupChar) == 1,
        nchar(groupChar) == 1
      )
      x %<>% gsub(groupChar, "", .)
    }
    x %<>%
      as.numeric()
    x[is_percentage] %<>% divide_by(100)
  }
  if (!is.null(unit) && "units" %in% rownames(utils::installed.packages())) {
    units::as.units(x, units::parse_unit(unit))
  } else {
    x
  }
}

#' @rdname parse_field
parse_integer <- function(x, unit = NULL) {
  if (!is.integer(x)) {
    if (!is.numeric(x)) {
      x %<>% as.numeric()
    }
    x_int <- as.integer(x)
    is_not_integer <- !is.na(x_int) & x != x_int
    if (any(is_not_integer)) {
      warning("NAs introduced by coercion")
      x_int[is_not_integer] <- NA_integer_
    }
    x <- x_int
  }
  if (!is.null(unit) && "units" %in% rownames(utils::installed.packages())) {
    units::as.units(x, units::parse_unit(unit))
  } else {
    x
  }
}

#' @rdname parse_field
#' @param trueValues (character) Values indicating \code{TRUE}.
#' @param falseValues (character) Values indicating \code{FALSE}.
parse_boolean <- function(x, trueValues = c("true", "True", "TRUE", "1"), falseValues = c("false", "False", "FALSE", "0")) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(trueValues) & is.numeric(falseValues) & !is.numeric(x)) {
    x %<>% as.numeric()
  }
  if (is.numeric(x)) {
    trueValues <- suppressWarnings(as.numeric(trueValues)) %>% stats::na.omit()
    falseValues <- suppressWarnings(as.numeric(falseValues)) %>% stats::na.omit()
  }
  if (length(trueValues) == 1) {
    is_true <- x == trueValues
  } else {
    is_true <- (x %in% trueValues) %>% replace(is.na(x), NA)
  }
  if (length(falseValues) == 1) {
    is_false <- x == falseValues
  } else {
    is_false <- x %in% falseValues
  }
  not_found <- which(is_true == is_false)
  if (length(not_found)) {
    warning("NAs introduced by coercion")
    is_true[not_found] <- NA
  }
  is_true
}

#' @rdname parse_field
#' @param format (character) Format specification (see \code{\link[base]{strptime}}).
parse_date <- function(x, format = "%Y-%m-%d") {
  if (inherits(x, "Date")) {
    x
  } else {
    if (length(format) == 0) {
      format <- "default"
    }
    stopifnot(
      is.character(format),
      length(format) == 1
    )
    format %<>% switch(
      default = "%Y-%m-%d",
      any = "",
      format
    )
    readr::parse_date(x, format = format, na = character()) %>%
      `attr<-`("problems", NULL)
  }
}

#' @rdname parse_field
parse_datetime <- function(x, format = "%Y-%m-%dT%H:%M:%SZ") {
  if (inherits(x, "Date")) {
    x %<>% as.POSIXlt(tz = "UTC")
  }
  if (inherits(x, "POSIXlt")) {
    x %<>% as.POSIXct()
  }
  if (inherits(x, "POSIXct")) {
    x
  } else {
    if (length(format) == 0) {
      format <- "default"
    }
    stopifnot(
      is.character(format),
      length(format) == 1
    )
    format %<>% switch(
      default = "%Y-%m-%dT%H:%M:%SZ",
      any = "",
      format
    )
    readr::parse_datetime(x, format = format, na = character(), locale = readr::locale(tz = "UTC")) %>%
      `attr<-`("problems", NULL)
  }
}

# ---- Field list ----

#' Parse list of fields
#'
#' @param l (list) Atomic vectors to parse.
#' @param meta (list of named lists) Field metadata.
#' @references \url{http://specs.frictionlessdata.io/tabular-data-resource/}
#' @family field parsers
parse_fields <- function(l, meta = list()) {
  field_names <- lapply(meta, "[[", "name")
  if (!setequal(field_names, names(l))) {
    stop(paste("Field names in data don't match names in metadata.\n", toString(names(l)), "\n", toString(field_names)))
  }
  for (i in seq_along(meta)) {
    l[[meta[[i]]$name]] %<>% parse_field(meta = meta[[i]])
  }
  l
}

# ---- Resource ----

#' Parse resource
#'
#' @details
#' NOTES:
#' \itemize{
#'   \item Assumes tabular data if the schema contains one or more fields.
#'   \item Only supports json and csv.
#' }
#'
#' @param meta (list) Resource metadata.
#' @param path (character) Path to data package.
#' @references
#' \url{https://specs.frictionlessdata.io/data-resource/},
#' \url{https://specs.frictionlessdata.io/tabular-data-resource/}
#' @family resource parsers
parse_resource <- function(meta, path = getwd()) {
  stopifnot(xor(is.null(meta$data), is.null(meta$path)))
  if (!is.null(meta$data)) {
    # Inline data
    # https://specs.frictionlessdata.io/data-resource/#data-inline-data
    x <- meta$data
    meta$data <- NULL
  } else {
    # Infile data (JSON, CSV, ... | multiple paths)
    x <- meta$path %>% ifelse(is_url(.), ., file.path(path, .))
  }
  x <- parse_resource_data(x, meta)
  if (!is.null(meta$schema)) {
    meta$schema$fields <- NULL
  }
  if (length(meta$schema) == 0) {
    meta$schema <- NULL
  }
  do.call(set_resource, c(list(x), meta))
}

#' Parse resource data
#'
#' @param x (character, list) Object to parse.
#' @param meta (named list) Resource metadata.
#' @references \url{https://specs.frictionlessdata.io/data-resource/}
#' @family resource parsers
parse_resource_data <- function(x, meta) {
  is_tabular <- length(meta$schema$fields) > 0
  format <- parse_resource_format(meta)
  parser <- function(x) {
    switch(
      format,
      json = read_json(x, tabular = is_tabular),
      csv = read_csv(x, na = meta$schema$missingValues, colClasses = if (is_tabular) "character")
    )
  }
  if (is.character(x)) {
    if (length(x) > 1) {
      x %<>% lapply(parser)
    } else {
      x %<>% parser()
    }
  } else {
    if (format == "json" && is_tabular) {
      x %<>% tabulate_json()
    }
  }
  if (is_tabular && rlang::is_bare_list(x)) {
    x %<>%
      data.table::rbindlist() %>%
      as.data.frame()
  }
  if (is_tabular) {
    x %<>%
      parse_fields(meta = meta$schema$fields)
  }
  x
}

#' Parse resource format
#'
#' @param meta (character) Resource metadata.
#' @references \url{https://specs.frictionlessdata.io/data-resource/}
#' @family resource parsers
parse_resource_format <- function(meta) {
  supported <- c("json", "csv")
  formats <- meta$format
  # Get format from mediatype
  if (!is.null(meta$mediatype)) {
    formats %<>% append(switch(
      meta$mediatype,
      "application/json" = "json",
      "text/csv" = "csv",
      meta$mediatype
    ))
  }
  # Get format from file extensions
  if (!is.null(meta$path)) {
    formats %<>% append(
      tools::file_ext(meta$path) %>%
        extract(. != "")
    )
  }
  # Validate and return result
  formats %<>% unique()
  if (length(formats) == 0) {
    if (!is.null(meta$path)) {
      stop(paste0("Unknown format for path: ", toString(meta$path)))
    }
    # Inline JSON object?
    "json"
  } else if (length(formats) == 1) {
    if (!formats %in% supported) {
      stop(paste0("Unsupported format: ", formats))
    }
    formats
  } else {
    stop(paste0("Inconsistent format specificatons: ", toString(c(meta$format, meta$mediatype, meta$path))))
  }
}
