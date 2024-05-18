\name{Rdo_args2txt}
\alias{Rdo_args2txt}
\title{
  Extract the descriptions of one or more arguments of a function
}
\description{
  Extract help descriptions of one or more arguments of a function
  and return them as a string.
}
\usage{
Rdo_args2txt(rdo, arg, title = "Hhh", name = "Aa", type = "text")
}
\arguments{
  \item{rdo}{the documentation for the topic, typically an Rd object but
    may be anything that \code{\link{Rd_fun}} accepts: Rd object, name
    of a function, or the value returned by \code{help}.}
  \item{arg}{name(s) of argument(s) to describe, a character vector, see
    also Details section}
  \item{title}{Title, a string}
  \item{name}{name, a string}
  \item{type}{type of the help, defaults to "text"}
}
\details{
  The \code{title} and \code{name} fields are there, since descriptions of
  arguments usually do not use the same  header as the description of
  the corresponding function.

  The current defaults show that this is still not finished.
}
\value{
  A string (character vector of length one).
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Georgi N. Boshnakov}

% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
\code{\link{Rdo_args2txt_list}}
}
\examples{
# ?seq
cat(Rdo_args2txt("seq", c("by", "...")))
cat(Rdo_args2txt("seq", c("from", "by")))
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
