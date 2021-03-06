% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_objects.R
\name{contributor}
\alias{contributor}
\title{Create contributor}
\usage{
contributor(title, path = NULL, email = NULL, role = NULL,
  organisation = NULL)
}
\arguments{
\item{title}{(character) Name or title of the contributor.}

\item{path}{(character) URL pointing to a relevant location online.}

\item{email}{(character) Email address.}

\item{role}{(character) String describing contributor's role (e.g. author, publisher, maintainer, wrangler, contributor).}

\item{organisation}{(character) Organisation contributor is affiliated with.}
}
\description{
A person or organization who contributed to the data package.
}
\examples{
contributor(
  title = "Joe Bloggs",
  email = "joe@bloggs.com",
  path = "http://www.bloggs.com",
  role = "author"
)
}
\references{
\url{https://specs.frictionlessdata.io/data-package/#contributors}
}
\seealso{
Other meta objects: \code{\link{constraints}},
  \code{\link{field}}, \code{\link{foreignKey}},
  \code{\link{package}}, \code{\link{resource}},
  \code{\link{schema}}, \code{\link{source}}
}
