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
  \code{Rdo_set_sectag} sets atrribute "Rd_tag" of the object \code{s}
  to \code{eltag}, then wraps \code{s} in \code{list()} with "Rd_tag"
  attribute \code{sectag}.

  The remaining functions provide one-argument access for some
  frequently used special cases.  \code{eltag} is "TEXT" for
  \code{Rd_title} and "VERB" for \code{Rd_name} and \code{Rd_args}.  The
  values of \code{sectag} are \verb{\title}, \verb{\name} and
  \verb{\arguments}, respectively.
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

"a" \%in\% letters

# to do: more examples
}
\keyword{ Rd }
