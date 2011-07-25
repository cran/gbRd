\name{Rd_help2txt}
\alias{Rd_help2txt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract selected help sections as text.
}
\description{
  Extract selected help sections as text.
}
\usage{
Rd_help2txt(x, topic, pkgname = "", help_type = "text",
               verbose = FALSE, try.all.packages = FALSE,
               keep_section = TRUE, omit_sec_header = FALSE)
}
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
  \item{omit_sec_header}{whether to omit or not the section header}
}
\details{
  Basically, this function calls \code{Rd_fun} to get an Rd object
  containing the required help sections, then converts them to text with
  \code{tools::Rd2txt}. At this point however unwanted sections may be
  present since \code{tools::Rd2txt} requires "\\title" and "\\name".
  If "\\title" is not an element of \code{keep_section}, it should be
  dropped. Other header information is dropped if \code{omit_sec_header
    = TRUE}.  The way this is done is crude and based on inspection. It
  would be better done using the Rd object but then I might need to,
  effectively reprogram \code{Rd2txt}.

  FIXME: The above was done for version R-2.10 (I think), see if a more
  modular version is available in current versions of \R.
  Also, it is tested only with \code{help_type="text"}.
}
\value{
  A character vector containing the text of the selected help sections.
}
% \references{
% %% ~put references to the literature/web site here ~
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
# 1st arg is name of a function
Rd_help2txt("data.frame",keep_section="\\\\arguments")
Rd_help2txt("seq",keep_section="\\\\examples")
Rd_help2txt("seq",keep_section="\\\\examples",omit_sec_header=TRUE)


}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ Rd }
