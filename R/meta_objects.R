# ---- Table Schema ----

#' Create table schema
#'
#' A schema declaration for a tabular data resource.
#'
#' @details
#' TODO: Support writing schemas to file with a path argument.
#'
#' @export
#' @param fields (list of named lists) See \code{\link{field}}. An empty \code{fields} is permitted to allow use with a \code{\link{resource}} data object.
#' @param missingValues (character) Strings treated as \code{NA} (see \href{https://specs.frictionlessdata.io/table-schema/#missing-values}{table-schema/missing-values}).
#' @param primaryKey (character) Name of primary key field(s) (see \href{https://specs.frictionlessdata.io/table-schema/#primary-key}{table-schema/primary-key}).
#' @param foreignKeys (list of named lists) See \code{\link{foreignKey}}.
#' @param ... (name = value) Additional properties.
#' @references \url{https://specs.frictionlessdata.io/table-schema/}
#' @family meta objects
#' @examples
#' schema(
#'   fields = list(
#'     field(name = "id", type = "integer"),
#'     field(name = "value", type = "number")
#'   ),
#'   missingValues = c("", "NA"),
#'   primaryKey = "id",
#'   tableSchema = TRUE
#' )
schema <- function(fields = list(), missingValues = NULL, primaryKey = NULL, foreignKeys = list(), ...) {
  get_called_args(...)
}

# ---- Table Schema: Fields ----

#' Create field metadata
#'
#' The metadata for a field. See \code{\link{set_field}} and \code{\link{get_field}} to set and get field metadata to and from an object.
#'
#' @export
#' @param name (character) Field name.
#' @param type (character) Field type (see \href{https://specs.frictionlessdata.io/table-schema/#types-and-formats}{table-schema/types-and-formats}).
#' @param format (character) Field format (see \href{https://specs.frictionlessdata.io/table-schema/#types-and-formats}{table-schema/types-and-formats}).
#' @param title (character) Human-readable title.
#' @param description (character) Human-readable description.
#' @param rdfType (character) URI of an RDF class (see \href{https://specs.frictionlessdata.io/table-schema/#rich-types}{table-schema/rich-types}).
#' @param constraints (named list) See \code{\link{constraints}}.
#' @param unit (character) Unit of measurement.
#' @param ... (name = value) Additional properties.
#' @references \url{https://specs.frictionlessdata.io/table-schema/#field-descriptors}
#' @family meta objects
#' @family field functions
#' @examples
#' field(
#'  name = "date_created",
#'  type = "date",
#'  format = "%Y-%m-%d"
#' )
field <- function(name = NULL, type = "string", format = NULL, title = NULL, description = NULL, rdfType = NULL, constraints = list(), unit = NULL, ...) {
  get_called_args(...)
}

#' Create field constraints
#'
#' A list of constraints for validating field values.
#'
#' @export
#' @param required (logical) Whether field cannot contain \code{NA}.
#' @param unique (logical) Whether field values must be unique.
#' @param minLength (integer) Minimum length of field values.
#' @param maxLength (integer) Maximum length of field values.
#' @param minimum Minimum value (of same type as field).
#' @param maximum Maximum value (of same type as field).
#' @param pattern (character) Regular expression field values must match (see \url{https://www.w3.org/TR/xmlschema-2/#regexs}).
#' @param enum (vector) Allowed values.
#' @references \url{https://specs.frictionlessdata.io/table-schema/#constraints}
#' @family meta objects
#' @examples
#' constraints(
#'   required = TRUE,
#'   unique = TRUE,
#'   minimum = "1970-01-01",
#'   pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
#' )
constraints <- function(required = NULL, unique = NULL, minLength = NULL, maxLength = NULL, minimum = NULL, maximum = NULL, pattern = NULL, enum = NULL) {
  get_called_args()
}

# ---- Table Schema: Other Properties ----

#' Create foreign key
#'
#' A reference between fields in different data package resources (tables).
#'
#' @export
#' @param fields (character) Local field name(s).
#' @param resource (character) Foreign resource name (within the same data package) or \code{""} for the local resource.
#' @param resource_fields (character) Foreign field name(s). Must match length of \code{fields}.
#' @references \url{https://specs.frictionlessdata.io/table-schema/#foreign-keys}
#' @family meta objects
#' @examples
#' foreignKey(
#'   fields = "state-code",
#'   resource = "state-codes",
#'   resource_fields = "code"
#' )
foreignKey <- function(fields, resource, resource_fields) {
  stopifnot(length(fields) == length(resource_fields))
  list(
    fields = fields,
    reference = list(
      resource = resource,
      fields = resource_fields)
  )
}

# ---- Data Resource ----

#' Create resource metadata
#'
#' The metadata for a data resource. See \code{\link{set_resource}} and \code{\link{get_resource}} to set and get resource metadata to and from an object.
#'
#' @export
#' @param name (character) Resource name.
#' @param path (character) URLs or paths relative to the data package directory (see \href{https://specs.frictionlessdata.io/data-resource/#path-data-in-files}{data-resource/path-data-in-files}).
#' @param profile (character) Custom resource profile (see \url{https://specs.frictionlessdata.io/profiles/}).
#' @param title (character) Human-readable title.
#' @param description (character) Human-readable description.
#' @param format (character) Output format (e.g. "csv", "json").
#' @param mediatype (character) Mediatype/mimetype of the output (e.g. "text/csv", "application/json").
#' @param encoding (character) Character encoding of the output (see \url{http://www.iana.org/assignments/character-sets/character-sets.xhtml}).
#' @param schema (named list) See \code{\link{schema}}.
#' @param bytes (integer) Size of the output file in bytes.
#' @param hash (character) MD5 hash of the output file.
#' @param sources (list) See \code{\link{source}}.
#' @param licenses (list) See \code{\link{license}}.
#' @param ... (name = value) Additional properties.
#' @references \url{https://specs.frictionlessdata.io/data-resource/}
#' @family meta objects
#' @family resource functions
#' @examples
#' resource(
#'  name = "data",
#'  path = "data/data.csv",
#'  format = "csv",
#'  schema = schema(missingValues = "")
#' )
resource <- function(name = NULL, path = NULL, profile = NULL, title = NULL, description = NULL, format = NULL, mediatype = NULL, encoding = "UTF-8", schema = list(), bytes = NULL, hash = NULL, sources = list(), licenses = list(), ...) {
  get_called_args(...)
}

# ---- Data Package ----

#' Create package metadata
#'
#' The metadata for a data package. See \code{\link{set_package}} and \code{\link{get_package}} to set and get package metadata to and from an object.
#'
#' @details
#' TODO:
#' \itemize{
#'   \item Require name?
#'   \item Name must be lower-case and contain only alphanumeric characters and \code{.}, \code{_}, and \code{-}.
#' }
#'
#' @export
#' @param name (character) Package name.
#' @param title (character) Human-readable title.
#' @param description (character) Human-readable description.
#' @param homepage (character) URL of the package homepage.
#' @param id (character) Globally unique identifier.
#' @param profile (character) Custom package profile (see \url{https://specs.frictionlessdata.io/profiles/}).
#' @param version (character) Version specification (see \url{http://semver.org/}).
#' @param created (character) ISO 8601 date and time of package creation (see \url{https://tools.ietf.org/html/rfc3339#section-5.6}).
#' @param sources (list) See \code{\link{source}}.
#' @param contributors (list) See \code{\link{contributor}}.
#' @param licenses (list) See \code{\link{license}}.
#' @param keywords (character) Keywords describing the package.
#' @param image (character) URL or path relative to the package directory (see \href{https://specs.frictionlessdata.io/data-resource/#url-or-path}{data-resource/url-or-path}).
#' @param resources (list) See \code{\link{resource}}.
#' @param ... (name = value) Additional properties.
#' @references \url{https://specs.frictionlessdata.io/data-package/}
#' @family meta objects
#' @family package functions
#' @examples
#' package(
#'  name = "example",
#'  title = "Example data package"
#' )
package <- function(name = NULL, title = NULL, description = NULL, homepage = NULL, id = NULL, profile = NULL, version = NULL, created = NULL, sources = list(), contributors = list(), licenses = list(), keywords = NULL, image = NULL, resources = list(), ...) {
  get_called_args(...)
}

#' Create license
#'
#' A license under which the data package is provided.
#'
#' @export
#' @param name (character) Open Definition license ID (see \url{http://licenses.opendefinition.org/}). Required if \code{path} is \code{NULL}.
#' @param path (character) URL or path (see \href{https://specs.frictionlessdata.io/data-resource/#url-or-path}{data-resource/url-or-path}). Required if \code{name} is \code{NULL}.
#' @param title (character) Human-readable title.
#' @references \url{https://specs.frictionlessdata.io/data-package/#licenses}
#' @family Meta objects
#' @examples
#' license(
#'   name = "ODC-PDDL-1.0",
#'   path = "http://opendatacommons.org/licenses/pddl/",
#'   title = "Open Data Commons Public Domain Dedication and License v1.0"
#' )
license <- function(name = NULL, path = NULL, title = NULL) {
  stopifnot(length(c(name, path)) > 0)
  get_called_args()
}

#' Create source
#'
#' A source of raw data for the data package.
#'
#' @export
#' @param title (character) Name of the source.
#' @param path (character) URL or path (see \href{https://specs.frictionlessdata.io/data-resource/#url-or-path}{data-resource/url-or-path}).
#' @param email (character) Email address.
#' @references \url{https://specs.frictionlessdata.io/data-package/#sources}
#' @family meta objects
#' @examples
#' source(
#'   title = "World Bank and OECD",
#'   path = "http://data.worldbank.org/indicator/NY.GDP.MKTP.CD"
#' )
source <- function(title = NULL, path = NULL, email = NULL) {
  get_called_args()
}

#' Create contributor
#'
#' A person or organization who contributed to the data package.
#'
#' @export
#' @param title (character) Name or title of the contributor.
#' @param url (character) URL pointing to a relevant location online.
#' @param email (character) Email address.
#' @param role (character) String describing contributor's role (e.g. author, publisher, maintainer, wrangler, contributor).
#' @references \url{https://specs.frictionlessdata.io/data-package/#contributors}
#' @family meta objects
#' @examples
#' contributor(
#'   title = "Joe Bloggs",
#'   email = "joe@bloggs.com",
#'   url = "http://www.bloggs.com",
#'   role = "author"
#' )
contributor <- function(title, url = NULL, email = NULL, role = NULL) {
  get_called_args()
}
