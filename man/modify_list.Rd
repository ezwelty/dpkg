% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{modify_list}
\alias{modify_list}
\title{Modify elements of a list
TODO: Modify nameless by position (delete with NULL) and append beyond length}
\usage{
modify_list(x, val, keep.null = FALSE, replace.nameless = TRUE)
}
\arguments{
\item{x}{(list)}

\item{val}{(list)}

\item{keep.null}{(logical) Whether \code{NULL} elements in \code{val} become \code{NULL} elements in \code{x} or are deleted.}

\item{replace.nameless}{(logical) Whether nameless list elements of \code{x} are replaced by corresponding elements of \code{val}.}
}
\description{
Modify elements of a list
TODO: Modify nameless by position (delete with NULL) and append beyond length
}
