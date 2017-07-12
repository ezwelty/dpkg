#' dpkg: Data Packages for R
#'
#' @docType package
#' @name dpkg
#' @import magrittr
NULL

# Quiets concerns of R CMD CHECK: no visible binding for global variable '.'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}
