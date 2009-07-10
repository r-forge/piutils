################################################################################
# R PACKAGE:   piUtils
# FILE:        test/test_MatchSpec.R
# DESCRIPTION: Automatic tests for MatchSpec.
#	           Runs when building the package. Its output is stored on 
#              <test_llevels.Rout> and compared with <test_llevels.Rout.save> 
#              if it exists. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  6 April 2009
# LICENSE:     GPL-2
################################################################################

# .libPaths(c(file.path(R.home(), "dev-library"), .libPaths()));
# library(piUtils);

(ms1 <- MatchSpec());
(ms2 <- new("MatchSpec", fun="match"));
(ms3 <- MatchSpec("match", nomatch=-1));
(ms4 <- MatchSpec("match", match.case=FALSE));
x <- c("a", "b", "c");
choices <- c("A", "C", "b", "ab");

identical(ms1, ms2);	# TRUE

resolve(ms1, data=x, table=choices);				# c(NA, 3, NA)
resolve(ms1, x, choices);						# c(NA, 3, NA)
doMatch("match", x, choices);					# c(NA, 3, NA)
doMatch(ms1, x, choices);						# c(NA, 3, NA)

resolve(ms2, x, choices, nomatch=-1);			# c(-1, 3, -1)
resolve(ms3, x, choices);						# c(-1, 3, -1)

resolve(ms1, x, choices, match.case=FALSE);					# c(1, 3, 2)
resolve(ms4, x, choices);									# c(1, 3, 2)
doMatch("match", x=x, table=choices, match.case=FALSE);		# c(1, 3, 2)


