`[.dpkg` <- function(x, i, j, ..., drop = TRUE) {
  structure(
    NextMethod(),
    dpkg_field = attr(x, "dpkg_field"),
    dpkg_resource = attr(x, "dpkg_resource"),
    dpkg_package = attr(x, "dpkg_package"),
    class = class(x)
  )
}

# `[[.dpkg` <- function(x, i, j, ..., exact = TRUE) {
#   structure(
#     NextMethod(),
#     dpkg_field = attr(x, "dpkg_field"),
#     dpkg_resource = attr(x, "dpkg_resource"),
#     dpkg_package = attr(x, "dpkg_package"),
#     class = class(x)
#   )
# }

append <- function(x, values, after = length(x)) {
  UseMethod("append")
}

append.default <- function(x, values, after = length(x)) {
  base::append(x, values = values, after = after)
}

append.dpkg <- function(x, values, after = length(x)) {
  r <- base::append(x, values = values, after = after)
  mostattributes(r) <- attributes(x)
  class(r) <- class(x)
  r
}
