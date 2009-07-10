################################################################################
# R PACKAGE:   piUtils
# FILE:        R/runit_misc.R
# DESCRIPTION: Test suite for miscellaneous functions. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  10/06/2009
# LICENSE:     GPL-2
################################################################################


test_hasNames <- svTest(function() {
	checkEquals(c(FALSE, FALSE), hasNames(1:2),
		"NULL names is detected");
	
	checkEquals(c(FALSE, TRUE), hasNames(list(1, B=2)),
		"empty string name is detected");
	
	checkEquals(c(TRUE, FALSE), hasNames(structure(1:2, names="A")),
		"NA name is detected");
	
	checkEquals(logical(0), hasNames(NULL), 
		"names of NULL returns a zero-length logical");
})

test_hasNames_dim <- svTest(function() {	
	x <- matrix(1:4, 2, 2);
	colnames(x) <- c("a", NA);
	
	checkEquals(rep(FALSE, 4), hasNames(x), "hasNames uses length by default");	
	checkEquals(rep(FALSE, 2), hasNames(x, dim=1), "hasNames with dim=1");
	checkEquals(c(TRUE, FALSE), hasNames(x, dim=2), "hasNames with dim=2");
}) 
    
test_emptyNamesFrom <- svTest(function() {
	checkEquals(c("a", "b"), emptyNamesFrom(1:2, c("a", "b", "c")),
		"get names when NULL")
	
	checkEquals(c("a", ""), emptyNamesFrom(1:2, c("a")),
		"get available names, leave rest empty");
	
	checkEquals(c("a", ""), emptyNamesFrom(1:2, c("a")),
		"get available names, leave rest empty");
	
	checkEquals(c("c", "a"), emptyNamesFrom(list(1, a=2), c("a", "c", "b")),
		"get available names not using already assigned");

	checkEquals(c("b", "j", NA_character_), 
		emptyNamesFrom(structure(1:3, names="b"), c("j", "", "b", "b", NA)),
		"get available names: unique, non-NA, values are used");

	checkEquals(NULL, emptyNamesFrom(1:3, NULL), "NULL names");
	checkEquals(NULL, emptyNamesFrom(1:3, c(NA, "")), "Only NA or empty names");
})

test_expandClasses <- svTest(function() {
	checkEquals(
		list(c("numeric", "raw", "factor", "POSIXct", "POSIXlt", NA, "integer", 
			"integer", "logical", "character", NA , "Date", "complex")),
		suppressWarnings(expandClasses("nrfPtJiilcNDx")));
	
	checkEquals(list(rep(NA_character_, 3)), 
		suppressWarnings(expandClasses(123)));
})

test_expandLogical <- svTest(function() {
	checkEquals(
		list(c(TRUE, FALSE, NA, TRUE, FALSE, NA, NA, NA, NA)),
		suppressWarnings(expandLogical("tfnTFNjwl")));
	
	checkEquals(list(rep(NA, 3)), 
		suppressWarnings(expandLogical(123)));
})