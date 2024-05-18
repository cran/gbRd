Provides utilities for processing Rd objects and files.  Extract argument
descriptions and other parts of the help pages of functions.

`gbRd` is effectively superceded by `Rdpack`. However `gbRd` contains a call to
an internal function related to parsing Rd files, which is not easy to copy,
unless most of the Rd processing in package "utils" is copied, so Rdpack still
imports it.

