\name{Rdo_create}
\alias{Rdo_create}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a basic Rd object
}
\description{
  Create a basic Rd object with fields title, name and arguments.
}
\usage{
Rdo_create(arguments, title = "Dummy title", name = "dummy name")
}
\arguments{
  \item{arguments}{The \code{argument} field of an Rd object}
  \item{title}{the title, a string}
  \item{name}{the name, atring}
}
\details{
  This is an auxiliary function needed to prepare \code{arguments} for a
  call to \code{Rd_help2txt} since the latter works on Rd objects or text but
  not on Rd sections.
}
\value{
  an Rd object
}
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
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
require(tools)   # for Rd2txt
a1 <- Rdo_get_args("seq")
a1
Rdo_create(a1)
Rd2txt(Rdo_create(a1))

a2 <- Rdo_get_args("seq", c("from", "to", "by"))
a2
Rdo_create(a2)
Rd2txt(Rdo_create(a2))

}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
