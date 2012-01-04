\name{Rdo_get_args}
\alias{Rdo_get_args}
\alias{Rdo_get_arg}
\title{
  Extract argument description from a help topic
}
\description{
  Extract argument description from a help topic.
}
\usage{
Rdo_get_args(rd, args, ...)
Rdo_get_arg(rd, arg)
}
\arguments{
  \item{rd}{the documentation for the topic, typically an Rd object but
    may be anything that \code{\link{Rd_fun}} accepts.}
  \item{arg}{an argument name, a string}
  \item{args}{names of arguments to describe, a character vector, see
    also Details section}
  \item{\dots}{not used}
}
\details{
  If \code{arguments} is missing, descriptions of all arguments are
  returned.

  Effort is made to handle the case when two or more arguments are
  described in a single entry. In that case it is not possible to
  disentangle the description automatically. So, the description is
  returned as is. Also, only one copy of the description is returned,
  see the examples with the \code{from} and \code{to} arguments of
  function \code{seq}.

  The \code{...} argument is handled, as well, give it as the string
  \verb{...} in \code{args}.

  \code{Rdo_get_arg} simply calls \code{Rdo_get_args} and returns the
  first element of its value. This means that \code{arg} is expected to
  be of length one, but this is not enforced. Note also that
  \code{Rdo_get_arg} is not completely equivalent to calling
  \code{Rdo_get_args} with \code{length(args)=1}.
}
\value{
  For \code{Rdo_get_args}, an Rd fragment representing the (part of)
  help section \verb{\arguments} containing descriptions of the requested
  arguments.

  For \code{Rdo_get_arg} an Rd fragment representing the help for a
  single argument.
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
h1 <- help("seq")
Rdo_get_args(h1)
Rdo_get_args(h1,"by")
Rdo_get_args(h1,"length.out")
Rdo_get_args(h1,"...")
Rdo_get_args(h1,"from")
Rdo_get_args(h1,c("from","by"))
Rdo_get_args(h1,c("from", "to"))

Rdo_get_args("seq")
Rdo_get_args("seq","by")
Rdo_get_args("seq","length.out")
Rdo_get_args("seq","...")
Rdo_get_args("seq","from")
Rdo_get_args("seq",c("from","by"))
Rdo_get_args("seq",c("from", "to"))
}
\keyword{ Rd }
