\name{Rdo_args2txt_list}
\alias{Rdo_args2txt_list}
\title{
  Extract the descriptions of the arguments of a function
}
\description{
  Collect the descriptions of the arguments of a function in a named list
  with one element per argument.
}
\usage{
Rdo_args2txt_list(x, arg, ...)
}
\arguments{
  \item{x}{help object, may be any of the types that
    \code{\link{Rd_fun}} accepts: Rd object, name of a function, or the
    the value returned by \code{help}.}
  \item{arg}{
    A character vector naming the arguments to describe. If \code{arg}
    is missing, descriptions of all arguments are extracted.}
  \item{\dots}{additional arguments to pass to \code{\link{Rdo_args2txt}}}
}
\details{
  If several arguments are described in a single documentation entry,
  then the whole text of the entry is given for each of the arguments.
}
\value{
  A named list with one entry (a string) for each of the requested arguments.
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
\seealso{
  \code{\link{Rdo_args2txt}}
}
\examples{
# each arg always gets an individual entry in the list;
# compare:
Rdo_args2txt_list("seq", c("from", "to", "by"))
# to:
cat(Rdo_args2txt("seq", c("from", "to", "by")))

}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
