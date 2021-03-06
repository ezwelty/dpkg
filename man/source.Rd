% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_objects.R
\name{source}
\alias{source}
\title{Create source}
\usage{
source(title = NULL, path = NULL, email = NULL)
}
\arguments{
\item{title}{(character) Name of the source.}

\item{path}{(character) URL or path (see \href{https://specs.frictionlessdata.io/data-resource/#url-or-path}{data-resource/url-or-path}).}

\item{email}{(character) Email address.}
}
\description{
A source of raw data for the data package.
}
\examples{
source(
  title = "World Bank and OECD",
  path = "http://data.worldbank.org/indicator/NY.GDP.MKTP.CD"
)
}
\references{
\url{https://specs.frictionlessdata.io/data-package/#sources}
}
\seealso{
Other meta objects: \code{\link{constraints}},
  \code{\link{contributor}}, \code{\link{field}},
  \code{\link{foreignKey}}, \code{\link{package}},
  \code{\link{resource}}, \code{\link{schema}}
}
