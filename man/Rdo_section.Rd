\name{Rdo_section}
\alias{Rdo_section}
\title{
  Extract a section element from an Rd object or Rd fragment
}
\description{
  Extract a section element from an Rd object or Rd fragment.
}
\usage{
Rdo_section(rdo, sec)
}
\arguments{
  \item{rdo}{an Rd object or fragment}
  \item{sec}{the required section, a string}
}
\details{
  If the "Rd_tag" attribute of \code{rdo} is "Rd" the required section
  is extracted using \code{tools:::RdTags()}. Otherwise, if this
  attribute is equal to \code{sec}, then \code{rdo} is returned.

  In all other cases it is assumed that \code{rdo} is the contents of
  the required section, its "Rd_tag" attribute is set to \code{sec} and
  returned without further modification.
}
\value{
  An Rd fragment for use as section element of an Rd object
}
\author{Georgi N. Boshnakov}
\note{
  This function is essentially a wrapper of \code{tools:::RdTags} with
  the additional facility to accept certain fragments. It is intended for
  use by other functions which work with  Rd objects.
}
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
%
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
