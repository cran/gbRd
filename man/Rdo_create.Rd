\title{
  Create basic Rd objects
}
\name{Rdo_create}
\alias{Rdo_create}
\alias{Rdo_empty}
\keyword{ Rd }
\description{
  Create basic Rd objects with fields title, name and arguments.
}
\usage{
Rdo_create(arguments, title = "Dummy title", name = "dummy name")

Rdo_empty(rdtag)
}
\arguments{
  \item{arguments}{The \code{argument} field of an Rd object}
  \item{title}{the title, a string}
  \item{name}{the name, atring}
  \item{rdtag}{a value for "Rd_tag", a string.}
}
\details{
  \code{Rdo_create} is an auxiliary function used to prepare \code{arguments} for a
  call to \code{Rd_help2txt} since the latter works on Rd objects or text but
  not on Rd sections.

  \code{Rdo_empty} creates an empty object of class "Rd"  if
  \code{rdtag} is missing. If \code{rdtag} is supplied the object is a
  list with attribute "Rd_tag" set to \code{rdtag}.
}
\value{
  an Rd object or a list with attribute "Rd_tag".
}
\author{Georgi N. Boshnakov}
\examples{
require(tools)   # for Rd2txt
a1 <- Rdo_get_args("seq")
a1
Rdo_create(a1)
Rd2txt(Rdo_create(a1))

a2 <- Rdo_get_args("seq", c("from", "to", "by"))
a2
Rdo_create(a2)
Rd2txt(Rdo_create(a2))

Rdo_empty()
class(Rdo_empty())
str(Rdo_empty())
}
