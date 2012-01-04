\name{Rd_fun}
\alias{Rd_fun}
\title{
  Return all or selected sections of a help topic as an Rd object
}
\description{
  Return all or selected sections of a help topic as an Rd object.  The
  help topic may be an Rd object, a character string (for the help
  function), or the value returned by the help function.
}
\usage{
Rd_fun(x, topic, pkgname = "", help_type = "text", verbose = FALSE,
          try.all.packages = FALSE, keep_section = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the help object. Its class may be "Rd", "character", or
    "help_files_with_topic".}
  \item{topic}{unused, see Details}
  \item{pkgname}{unused, see Details}
  \item{help_type}{type of help, see Details and \code{\link{help}}.}
  \item{verbose}{logical value, see \code{\link{help}}.}
  \item{try.all.packages}{logical value, see \code{\link{help}}.}
  \item{keep_section}{the section(s) to keep. If it is a
    character vector of length at least one, the sections listed in it
    (plus \verb{\title} and \verb{\name}) are kept in the result, the rest are
    dropped. Otherwise all sections are returned.}
}
\details{
  If the class of \code{x} is neither "Rd" nor "help_files_with_topic",
  \code{x} is assumed to be appropriate for a call to
  \code{help}. The call is made to obtain an object of class
  "help_files_with_topic", which is then processed as below.
  Arguments \code{help_type}, \code{verbose} and \code{try.all.packages}
  are used only in this case.

  If the class of \code{x} is "help_files_with_topic"
  (usually the result of a call to \code{help}), then an Rd object is
  obtained using \code{tools:::fetchRdDB}.

  The Rd object (\code{x} itself or the one obtained as described above)
  is examined and sections are retained or dropped as specified by
  argument \code{keep_section}.
  Sections \verb{\title} and \verb{\name} are always kept in the
  returned value since otherwise the Rd object is considered invalid by
  (some of?) the system functions.

}
\value{
  an Rd object
}
\author{Georgi N. Boshnakov}
\note{
  Note that \code{help} works with `installed' help. So, when the Rd
  object is obtained via a call to help it will not necessarilly
  be the one that would be obtained from the original Rd file if that
  contains \verb{\Sexpr} instructions with \code{stage=build} or
  \code{stage=install} optional argument. This is not a problem for the
  intended purpose of this package to allow for extraction of pieces of
  the help for selective display and related run-time actions.
  For manipulation of source Rd files one can supply an Rd object
  obtained by \code{parse_Rd}-ying it.


  FIXME: I wrote this function in a hurry when it turned out that the
  help system has changed in R version 2.10, needs clean up.

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
Rd_fun("data.frame",keep_section="\\\\arguments")
Rd_fun("seq",keep_section="\\\\arguments")

# 1st arg is the value of a call to help()
h1 <- help("seq")
class(h1)
Rd_fun(h1,keep_section="\\\\title") # note: in Rd file the number of
Rd_fun(h1,keep_section="\\\\arguments") # backslashes is twice that in
                                        # the rendered doc.
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
