\name{Rd_title}
\alias{Rd_title}
\alias{Rd_name}
\alias{Rd_args}
\alias{Rdo_set_sectag}
\title{
  Wrap an object so that it can be used as a section element of an Rd object.
}
\description{
  Wrap an object so that it can be used as a section element of an Rd object.
}
\usage{
Rdo_set_sectag(s,sectag,eltag)
Rd_title(s)
Rd_name(s)
Rd_args(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{the object to be wrapped, often a string, see Details}
  \item{sectag}{the section tag, a string}
  \item{eltag}{the element tag, a string}
}
\details{
  Atrribute "Rd_tag" of the object \code{s} is set to \code{eltag}, then
  \code{s} is wrapped in \code{list()} whose "Rd_tag" attribute is set
  to \code{sectag}.  For the functions that do not have these arguments,
  these are hard-coded. \code{eltag} is "TEXT" for \code{Rd_title} and
  "VERB" for \code{Rd_name} and \code{Rd_args}.
  The values of \code{sectag} are "\\title", "\\name" and "\\arguments",
  respectively.
}
\value{
  A tagged list as described in Details.
}
\author{Georgi N. Boshnakov}

% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
Rd_title("My seq")
Rd_name("myseq")

# to do: more examples
}
\keyword{ Rd }
