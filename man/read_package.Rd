% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_package}
\alias{read_package}
\alias{read_package_github}
\title{Read data package}
\usage{
read_package(path = getwd(), resources = NULL)

read_package_github(repo, ...)
}
\arguments{
\item{path}{(character) Package directory or URL.}

\item{resources}{(character, numeric) Names or indices of resources to read (or all if \code{NULL}).}

\item{repo}{(character) GitHub repository address (see \code{\link{github_raw_url}}).}

\item{...}{Additional arguments passed to \code{\link{read_package}}.}
}
\description{
Read data package
}
\details{
NOTES:
\itemize{
  \item Only supports json and csv formats.
}
}
\references{
\url{https://specs.frictionlessdata.io/data-resource/}
}
\seealso{
Other package readers: \code{\link{read_data}},
  \code{\link{read_meta}}
}
