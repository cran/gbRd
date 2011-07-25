\name{Rd_fun}
\alias{Rd_fun}
\title{
  Return all or selected fields from a help topic as an Rd object
}
\description{
  Return all or selected fields from a help topic as an Rd object.
  The help topic may be an Rd object, a string as for the
  help function, or the value returned by the help function.
}
\usage{
Rd_fun(x, topic, pkgname = "", help_type = "text", verbose = FALSE,
          try.all.packages = FALSE, keep_section = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the help object. Its class may be "Rd", string or
    "help_files_with_topic".}
  \item{topic}{FIXME: unused !!!}
  \item{pkgname}{FIXME: unused !!!}
  \item{help_type}{Used only when function \code{help} is called,
    defaults to "text".}
  \item{verbose}{Used only when function \code{help} is called,
    defaults to \code{FALSE}.}
  \item{try.all.packages}{Used only when function \code{help} is called,
    defaults to \code{FALSE}.}
  \item{keep_section}{the section to keep. If it is a
    character vector of length at least one, the sections listed in it
    (plus "\\title" and "\\name") are kept in the result, the rest are
    dropped. Otherwise all sections are returned.}
}
\details{
  FIXME: I wrote this function in a hurry when it turned out that the
  help system has changed in R version 2.10, needs clean up.

  If \code{x} is already an Rd object, then \code{keep_section} is
  examined and some sections are dropped if it says so.

  If the class of \code{x} is "help_files_with_topic"
  (usually obtained by a call to \code{help}, then an Rd object is
  fetched using \code{tools:::fetchRdDB} and sections are retained or
  dropped as specified by argument \code{keep_section}.

  Otherwise \code{x} is assumed to be appropriate for a call to
  \code{help}. The call is made to obtain an object of class
  "help_files_with_topic", which is then processed as above.

  Sections "\\title" and "\\name" are always kept in the returned
  results since otherwise the Rd object is considered invalid by the
  system functions.
}
\value{
  an Rd object
}
\author{Georgi N. Boshnakov}
\note{
  This and may be some other functions from this package use internal
  functions and therefore are volatile to changes in core R.

  todo:
  In recent versions of R, \code{help} may return more than one file
  (see \code{paths} in this function's source), this needs to be
  handled.
}
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
# 1st arg is name of a function
Rd_fun("data.frame",keep_section="\\arguments")
Rd_fun("seq",keep_section="\\arguments")


# 1st arg is the value of a call to help()
h1 <- help("seq")
class(h1)
Rd_fun(h1,keep_section="\\title")
Rd_fun(h1,keep_section="\\arguments")


}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
