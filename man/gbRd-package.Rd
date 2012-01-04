\name{gbRd-package}
\alias{gbRd-package}
\alias{gbRd}
\docType{package}
\title{
Utilities for processing Rd objects and files
}
% Provides utilities for processing Rd objects and files.
% Extract argument descriptions and other parts of the help pages of
% functions.
%
% In the version of gbRd on 2011-10-31 (and 2.13 of R), if the
% description consists of the above lines, the `Details' header happens
% towards the top of the page but everything after it goes on the
% following page (leaving an almost empty page.
%

\description{
Provides utilities for processing Rd objects and files.
Extract argument descriptions and other parts of the help pages of
functions. The main purpose of the functions is to facilitate
extraction of descriptions of function arguments for presentation
of simplified usage descriptions.
}

\details{

\tabular{ll}{
Package: \tab gbRd\cr
Type: \tab Package\cr
Version: \tab 0.4-8\cr
Date: \tab 2011-10-31\cr
Depends: \tab tools\cr
License: \tab GPL2 or later\cr
LazyLoad: \tab yes\cr
Built: \tab R 2.13.1; ; 2011-10-31 09:21:19 UTC; windows\cr
}

Index:
\preformatted{
Rd_fun                  Return all or selected sections of a help topic
                        as an Rd object
Rd_help2txt             Extract selected help sections as text.
Rd_title                Wrap an object so that it can be used as a
                        section element of an Rd object.
Rdo_args2txt            Extract the descriptions of one or more
                        arguments of a function
Rdo_args2txt_list       Extract the descriptions of the arguments of a
                        function
Rdo_create              Create basic Rd objects
Rdo_get_args            Extract argument description from a help topic
Rdo_section             Extract a section element from an Rd object or
                        Rd fragment
gbRd-package            Utilities for processing Rd objects and files
}
}

\author{
Georgi N. Boshnakov

Maintainer: Georgi N. Boshnakov <georgi.boshnakov@manchester.ac.uk>
}
\keyword{ package }

