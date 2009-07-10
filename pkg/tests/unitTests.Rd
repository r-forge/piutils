%###############################################################################
% R PACKAGE:   ppTools
% FILE:	       man/unitTests.Rd
% DESCRIPTION: svUnit test suite for package ppTools, to be run via example() 
% AUTHOR: 	   Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
% CREATED ON:  15 April 2009
% LICENSE:     LGPL3
%###############################################################################

\name{unitTests}
\alias{unitTests.ppTools}
\title{ Unit tests for the package ppTools }
\description{ Performs unit tests defined in this
	package by running \code{example(unitTests.ppTools)}.
	Tests are in \code{runit*.R} files Located in the
	'/unitTests' subdirectory or one of its subdirectories 
	('/inst/unitTests' and subdirectories in package sources).
}
\author{Enrique Bengoechea 
	(\email{enrique.bengoechea@credit-suisse.com})}
\examples{
library(ppTools)
# Make sure to clear log of errors and failures first
clearLog()

# Run all test units defined in 'ppTools' package
(runTest(svSuite("package:ppTools"), "ppTools"))

# Tests enclosed within \donttest are ran with 
# example(unitTests.ppTools) but do not run with 
# R CMD check
\donttest{
	# Run all test units in subdirectory
	# /unitTests/VirtualClass 
#	(runTest(svSuite("package:ppTools (VirtualClass)"), 
#		"VirtualClass"))
}

# Check errors at the end of the process
#(needed to interrupt R CMD check)
errorLog()
}
\keyword{utilities}