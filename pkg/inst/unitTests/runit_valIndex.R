################################################################################
# R PACKAGE:   piUtils
# FILE:        R/runit_valIndex.R
# DESCRIPTION: Test suite for object indexes validation functions.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  22/06/2009
# LICENSE:     GPL-2
################################################################################
# TODO: Test handlers

test_valIndex_logical <- svTest(function() {
	checkEquals(logical(0), valIndex(NULL, 1:3));
	checkEquals(logical(0), valIndex(logical(0), 1:3));
	
	checkEquals(c(TRUE, TRUE, TRUE), valIndex(TRUE, 1:3));
	checkEquals(c(TRUE, FALSE, TRUE), valIndex(c(TRUE, FALSE), 1:3));
	checkEquals(c(TRUE, FALSE, FALSE), valIndex(c(TRUE, FALSE, FALSE), 1:3));
	
	checkException(valIndex(c(TRUE, FALSE, FALSE, TRUE), 1:3));	
})

test_valIndex_logicalScalar <- svTest(function() {
	checkEquals(c(FALSE, TRUE, FALSE), 
		valIndex(c(FALSE, TRUE, FALSE), 1:3, several.ok=FALSE));
	
	checkException(valIndex(c(TRUE, FALSE, TRUE), 1:3, several.ok=FALSE));
	
	checkEquals(c(FALSE, TRUE, FALSE),
		valIndex(c(FALSE, TRUE, TRUE), 1:3, several.ok=FALSE, handler=NULL))
})

test_valIndex_logicalDim <- svTest(function() {
	m <- matrix(1:6, nrow=2, ncol=3, dimnames=list(NULL, c("AX", "BX", "CX")));
		
	checkEquals(c(TRUE, FALSE, TRUE), 
		suppressWarnings(valIndex(c(TRUE, FALSE), m, dim=2)));
	checkException(valIndex(c(TRUE, FALSE, TRUE, FALSE), m, dim=2));

	checkEquals(c(FALSE, TRUE), valIndex(c(FALSE, TRUE), m, dim=1));	
	checkException(valIndex(c(FALSE, TRUE, FALSE), m, dim=1));
	
	# With several.ok = FALSE
	checkEquals(c(FALSE, TRUE),
		valIndex(c(FALSE, TRUE), m, dim=1, several.ok=FALSE));
	checkException(valIndex(c(TRUE, TRUE), m, dim=1, several.ok=FALSE));	
})

test_valIndex_logicalNew <- svTest(function() {
	checkEquals(structure(c(TRUE, FALSE), is.new=c(FALSE, FALSE)), 
		valIndex(c(TRUE, FALSE), 1:2, mark.new=TRUE, handler=NULL));
	
	checkEquals(structure(c(TRUE, FALSE, FALSE), is.new=c(FALSE, FALSE, TRUE)),
		suppressWarnings(valIndex(c(TRUE, FALSE, FALSE), 1:2, mark.new=TRUE, 
			handler="warning")));
		
	checkException(valIndex(c(TRUE, FALSE, FALSE), 1:2, mark.new=TRUE));	
})

test_valIndex_logicalRetclass <- svTest(function() {
	x <- setNames(1:2, c("a", "b"));
	checkEquals(c(FALSE, TRUE), 
		valIndex(c(FALSE, TRUE), x, retclass="logical"));

	checkEquals(structure(c(FALSE, TRUE, FALSE, TRUE), 
			is.new=c(FALSE, FALSE, TRUE, TRUE)),    	
		valIndex(c(FALSE, TRUE, FALSE, TRUE), x, retclass="logical", 
			mark.new=TRUE, handler=NULL));
		
	checkEquals(2L, valIndex(c(FALSE, TRUE), x, retclass="integer"));
	
	checkEquals(structure(c(2L, 4L), is.new=c(FALSE, TRUE)), 
		valIndex(c(FALSE, TRUE, FALSE, TRUE), x, retclass="integer",
			mark.new=TRUE, handler=NULL));
				
	checkEquals("b", valIndex(c(FALSE, TRUE), x, retclass="character"));
		
	checkException(valIndex(c(FALSE, TRUE), 1:2, retclass="character"));

	checkEquals(structure(c("b", NA_character_), is.new=c(FALSE, TRUE)),
		valIndex(c(FALSE, TRUE, TRUE), x, retclass="character", mark.new=TRUE,
			handler=NULL));
	
})

test_valIndex_integer <- svTest(function() {
	checkEquals(integer(0), valIndex(integer(0), 1:3));
		
	checkEquals(1L, valIndex(1, 1:3));
	checkEquals(-c(2L, 3L), valIndex(-c(2, 3), 1:3));
	checkEquals(-5L, valIndex(-5, 1:3));		
	
	checkException(valIndex(4, 1:3));
})

test_valIndex_integerScalar <- svTest(function() {
	checkEquals(2L, valIndex(2.0, 1:3, several.ok=FALSE));
	
	# BEWARE: This negative integer index is allowed because only one
	# element would be returned, but if used with the [[ operator it would
	# issue an error....		
	checkEquals(-c(1L, 2L), valIndex(-c(1, 2), 1:3, several.ok=FALSE));
	
	checkException(valIndex(c(1, 2), 1:3, several.ok=FALSE));		
	checkException(valIndex(-1, 1:3, several.ok=FALSE));
	
	checkEquals(2L,
		valIndex(c(2, 3), 1:3, several.ok=FALSE, handler=NULL));

	checkEquals(2L,
		valIndex(-1, 1:3, several.ok=FALSE, handler=NULL));
})

test_valIndex_integerDim <- svTest(function() {
	m <- matrix(1:6, nrow=2, ncol=3, dimnames=list(NULL, c("AX", "BX", "CX")));

	checkEquals(c(1L, 2L), valIndex(c(1, 2), m, dim=1));
	checkEquals(-1L, valIndex(-1, m, dim=1));
	
	checkException(valIndex(3, m, dim=1));
		
	# With several.ok = FALSE
	checkEquals(3L, valIndex(3, m, dim=2, several.ok=FALSE));
	checkException(valIndex(c(2, 3), m, dim=2, several.ok=FALSE));	
})

test_valIndex_integerNew <- svTest(function() {
	checkEquals(structure(c(2L, 4L), is.new=c(FALSE, TRUE)), 
		valIndex(c(2, 4), 1:2, mark.new=TRUE, handler=NULL));
	
	checkEquals(structure(2L, is.new=FALSE),
		suppressWarnings(valIndex(-1, 1:2, mark.new=TRUE, handler="warning")));

	checkEquals(structure(c(1L, 2L), is.new=c(FALSE, FALSE)),
		suppressWarnings(valIndex(-7, 1:2, mark.new=TRUE, handler=NULL)));
	
	checkException(valIndex(c(2, 4), 1:2, mark.new=TRUE));	
})
		
test_valIndex_integerRetClass <- svTest(function() {
	x <- setNames(1:2, c("a", "b"));
	checkEquals(1L, valIndex(1, x, retclass="integer"));
	checkEquals(1L, valIndex(-2, x, retclass="integer"));
	
	checkEquals(structure(c(FALSE, TRUE), is.new=c(FALSE, FALSE)), 
		valIndex(2, x, retclass="logical", mark.new=TRUE));
	checkEquals(structure(c(TRUE, FALSE), is.new=c(FALSE, FALSE)), 
		valIndex(-2, x, retclass="logical", mark.new=TRUE));
	checkEquals(structure(c(FALSE, FALSE, FALSE, TRUE), 
    		is.new=c(FALSE, FALSE, TRUE, TRUE)),
		valIndex(4, x, retclass="logical", mark.new=TRUE, handler=NULL));

	checkEquals(structure("b", is.new=FALSE),
		valIndex(2, x, retclass="character", mark.new=TRUE));
	checkEquals(structure("a", is.new=FALSE), 
		valIndex(-2, x, retclass="character", mark.new=TRUE));
	checkException(valIndex(-2, 1:2, retclass="character"));	
})
		
test_valIndex_character <- svTest(function() {
	checkEquals(character(0), valIndex(character(0), 1:3));
	
	x <- structure(1:3, names=c("ab", "ba", "cd"));
	checkEquals("ba", valIndex("b", x));
	checkEquals(c("ab", "cd"), valIndex(c("a", "c"), x));
	checkException(valIndex("j", x));
	checkException(valIndex("ab", 1:3));
	
	checkEquals(0L, valIndex(0, character(0)));
	checkException(valIndex(1, character(0)));	
})

test_valIndex_characterScalar <- svTest(function() {	
	x <- structure(1:3, names=c("ab", "ba", "cd"));
	
	checkEquals("ba", valIndex("ba", x, several.ok=FALSE, exact=TRUE));
	checkEquals("ba", valIndex("b", x, several.ok=FALSE));
	checkException(valIndex("b", x, several.ok=FALSE, exact=TRUE));	
	checkException(valIndex(c("a", "c"), x, several.ok=FALSE));
	
	checkEquals("ab", valIndex(c("a", "c"), x, several.ok=FALSE, handler=NULL));
})

test_valIndex_characterDim <- svTest(function() {
	m <- matrix(1:6, nrow=2, ncol=3, dimnames=list(NULL, c("AX", "BX", "CX")));
	
	checkException(valIndex("A", m, dim=1));
	
	checkEquals(c("CX", "AX"), valIndex(c("C", "A"), m, dim=2));
	checkException(valIndex("J", m, dim=2));
	
	# With several.ok = FALSE
	checkEquals("CX", valIndex("CX", m, dim=2, several.ok=FALSE));
	checkException(valIndex(c("AX", "CX"), m, dim=1, several.ok=FALSE));		
})

test_valIndex_characterNew <- svTest(function() {
	x <- structure(1:2, names=c("ab", "ba"));
	
	checkEquals(structure(c("ba", "cd"), is.new=c(FALSE, TRUE)), 
		valIndex(c("ba", "cd"), x, mark.new=TRUE, handler=NULL));

	checkEquals(structure(c("ba", "ab"), is.new=c(FALSE, FALSE)), 
		valIndex(c("b", "a"), x, mark.new=TRUE, exact=FALSE, handler=NULL));
	
	checkEquals(structure("jk", is.new=TRUE),
		suppressWarnings(valIndex("jk", x, mark.new=TRUE, handler="warning")));
	
	checkException(valIndex("jk", x, mark.new=TRUE));	
})

test_valIndex_characterRetClass <- svTest(function() {
	x <- setNames(1:2, c("ab", "ba"));
	checkEquals(c("ba", "ab"), valIndex(c("b", "a"), x, retclass="character"));

	checkEquals(c(2L, 1L), valIndex(c("b", "a"), x, retclass="integer"));
	checkEquals(structure(c(2L, 1L, 3L), is.new=c(FALSE, FALSE, TRUE)), 
		valIndex(c("b", "a", "c"), x, retclass="integer", mark.new=TRUE,
			handler=NULL));
	
	checkEquals(c(FALSE, TRUE), valIndex(c("ba"), x, retclass="logical"));
	checkEquals(structure(c(FALSE, TRUE, TRUE), is.new=c(FALSE, FALSE, TRUE)), 
		valIndex(c("b", "c"), x, retclass="logical", mark.new=TRUE,
			handler=NULL));
	
	
})