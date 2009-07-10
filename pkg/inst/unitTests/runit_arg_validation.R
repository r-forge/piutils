################################################################################
# R PACKAGE:   piUtils
# FILE:        inst/unitTests/runit_arg_validation.R
# DESCRIPTION: Test suite for argument validation functions.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  26/05/2009
# LICENSE:     GPL-2
################################################################################
# Run the suite simply by        example(unitTests.lfactor)
#      or set devPath() and      ut()
# Run individual tests with      (runTest(test_simplest_ltable()))

## Create a few objects we need for tests

# Executed before each test function
.setUp <- function () {
    ## Specific actions for svUnit: prepare context
    if ("package:svUnit" %in% search()) {
        .Log <- Log() ## Make sure .Log is created
        .Log$..Unit <- "C:/DOCUME~1/m824704/LOCALS~1/Temp/RtmpnJ8PsS/runitlt1.R"
        .Log$..File <- ""
        .Log$..Obj <- ""
        .Log$..Tag <- ""
        .Log$..Msg <- ""
        rm(..Test, envir = .Log)
    }
	
	#assign("firstLetters", LETTERS[4:1], envir=.GlobalEnv);	
}

# Executed after getOption("svUnit.excludeList")each test function
.tearDown <- function () {
    ## Specific actions for svUnit: clean up context
    if ("package:svUnit" %in% search()) {
        .Log$..Unit <- ""
        .Log$..File <- ""
        .Log$..Obj <- ""
        .Log$..Tag <- ""
        .Log$..Msg <- ""
        rm(..Test, envir = .Log)
    }
	
	#rm("firstLetters", envir=.GlobalEnv);
}

# Test function valClass() with default coerce=FALSE
test_valClass_noCoerce <- svTest(function() {
	checkEquals(1L, valClass(1L, "integer"), 
		"validate integer class argument");		
	checkEquals(NA_real_, valClass(NA_real_, "numeric"), 
		"validate numeric class argument");
	checkEquals(c(TRUE, NA), valClass(c(TRUE, NA), "logical"), 
		"validate logical class argument");
	checkEquals(c(NA_character_, "a"),  valClass(c(NA, "a"), "character"),
		"validate character class argument");
	checkEquals(as.Date("2000-01-01"), 
		valClass(as.Date("2000-01-01"), "Date"));
	x <- strptime("2006-01-08 10:07:52", "%Y-%m-%d %H:%M:%S");
	checkEquals(x, valClass(x, "POSIXt"), "validate POSIXt class argument");
	checkEquals(x, valClass(x, "POSIXlt"), "validate POSIXlt class argument");
	
	checkException(valClass(1L, "double"), "integer does not validate as double");
	checkException(valClass(1, "integer"), "double does not validate as integer");
	checkException(valClass("a", "logical"), 
		"character does not validate as logical");
	checkException(valClass(x, "Date"), "POSIXlt does not validate utas Date");	
})

# Test function valClass() with coerce=TRUE
test_valClass_coerce <- svTest(function() {
	checkEquals(c(1L, 2L), valClass(c(1.0, 2.1), "integer", coerce=TRUE), 
		"coerce argument from double to integer");	
	checkEquals(NA, valClass(NA_character_, "logical", coerce=TRUE), 
		"coerce argument from NA character to NA logical");
	checkEquals(c("1", "2"), valClass(c(1,2), "character", coerce=TRUE), 
		"coerce argument from numeric to character");
	
	x <- strptime("2006-01-08 10:07:52", "%Y-%m-%d %H:%M:%S");
	checkEquals(as.Date("2006-01-08"), valClass(x, "Date", coerce=TRUE), 
		"coerce argument from POSIXct to Date");
})

# Test function valClass() with different actions
test_valClass_handlers <- svTest(function() {
	produceWarning <- FALSE;
	tryCatch(valClass(c(1.0, 2.1), "integer", handler="warning"),
		error=function(e) 
			checkTrue(FALSE, "'warning' handler should not generate error"),
		message=function(e) 
			checkTrue(FALSE, "'warning' handler should not generate message"),
		warning=function(e) produceWarning <<- TRUE,
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='warning'"));
	checkTrue(produceWarning, "handler='warning' must generate warning");
	
	x <- suppressWarnings(valClass(c(1.0, 2.1), "integer", handler="warning"));
	checkEquals(c(1.0, 2.1), x, 
		"handler='warning' returns the data unmodified")
	
	produceMessage <- FALSE;
	tryCatch(valClass(c(1.0, 2.1), "integer", handler="message"),
		error=function(e) 
			checkTrue(FALSE, "'message' handler should not generate error"),
		warning=function(e) 
			checkTrue(FALSE, "'message' handler should not generate warning"),
		message=function(e) produceMessage <<- TRUE,
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='message'"));
	checkTrue(produceMessage, "handler='message' must generate message");
	
	x <- suppressMessages(valClass(c(1.0, 2.1), "integer", handler="message"));
	checkEquals(c(1.0, 2.1), x, 
		"handler='message' returns the data unmodified")	
})

# Test that function valClass() correctly reports the original function 
# and argument name
test_valClass_message <- svTest(function() {
	myTestFunction <- function(myTestArg) valClass(myTestArg, "logical");
	
	tryCatch(myTestFunction(1),
		error=function(e) {
			checkTrue(length(grep("myTestFunction", as.character(e))) > 0,
				"original function name must appear in valClass error message");
			checkTrue(length(grep("myTestArg", as.character(e))) > 0,
				"original argument name must appear in valClass error message");			
		}
	)		
})

# Test function valLength() with default recycle=FALSE
test_valLength_noRecycle <- svTest(function() {
	checkEquals(2L, valLength(2L, 1), "validate simple length");	
	checkEquals(c(TRUE, FALSE), valLength(c(TRUE, FALSE), c(2, 0)), 
		"validate multiple lengths, case 1");
	checkEquals(logical(0), valLength(logical(0), c(2, 0)), 
		"validate multiple lengths, case 2 (zero-length)");
	checkEquals("a", valLength("a", NA), 
		"validate len=NA with length(x)=1");
	checkEquals(c("a", "b"), valLength(c("a", "b"), NA), 
		"validate len=NA with length(x)=2");
	    
	checkException(valLength(2L, 2), "invalid length raises error");
	checkException(valLength(TRUE, c(2, 0)), 
		"invalid length with multiple lengths raises errors");
	checkException(valLength(character(0), NA), 
		"zero length is not allowed when len=NA");		
})

# Test function valLength() with recycle=FALSE
test_valLength_recycle <- svTest(function() {
	checkEquals(c(2L, 1L), valLength(c(2L, 1L), 2, recycle=TRUE), 
		"validate simple length with recycle=TRUE, without recycling");	
	checkEquals(c(TRUE, FALSE, TRUE, FALSE), 
		valLength(c(TRUE, FALSE), 4, TRUE), 
		"validate multiple lengths with correct recycling");
	suppressWarnings(checkEquals(c(TRUE, FALSE, TRUE), 
		valLength(c(TRUE, FALSE), 3, TRUE), 
		"validate multiple lengths with non-multiple length recycling"));
	suppressWarnings(checkEquals(c("a"), 
		valLength(c("a", "b", "c"), c(0, 1), recycle=TRUE), 
		"validate multiple lengths with non-multiple length recycling"));
	
	checkException(valLength(numeric(0), 2, recycle=TRUE), 
		"invalid length with recycle=TRUE raises error");
	checkException(valLength(character(0), NA, recycle=TRUE), 
		"zero length is not allowed when len=NA & recycle=TRUE");		
})

# Test function valLength() with different condition handlers
test_valLength_handlers <- svTest(function() {
	x <- tryCatch(valLength(c(1.0, 2.1), 1, handler="warning"),
		error=function(e) 
			checkTrue(FALSE, "'warning' handler should not generate error"),
		message=function(e) 
			checkTrue(FALSE, "'warning' handler should not generate message"),
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='warning'"));
	checkEquals(c(1.0, 2.1), x, 
		"handler='warning' returns the data unmodified")
	
	tryCatch(valLength(c(1.0, 2.1), 1, handler="message"),
		error=function(e) 
			checkTrue(FALSE, "'message' handler should not generate error"),
		warning=function(e) 
			checkTrue(FALSE, "'message' handler should not generate warning"),
		message=function(e){},
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='message'"));
	checkEquals(c(1.0, 2.1), x, 
		"handler='message' returns the data unmodified")	
})

# Test that function valLength() correctly reports the original function 
# and argument name
test_valLength_message <- svTest(function() {
	myTestFunction <- function(myTestArg) valLength(myTestArg, 2);
	
	tryCatch(myTestFunction(1),
		error=function(e) {
			checkTrue(length(grep("myTestFunction", as.character(e))) > 0,
				"original function name must appear in valClass error message");
			checkTrue(length(grep("myTestArg", as.character(e))) > 0,
				"original argument name must appear in valClass error message");			
		}
	)		
})

# Test function valChoice()
test_valChoice <- svTest(function() {
	checkEquals("ab", valChoice("a", c("ba", "ab")), "standard single match");	
	checkEquals("ba", valChoice(NULL, c("ba", "ab")), 
		"single match with arg=NULL");
	
	checkException(valChoice("a", c("ba", "ab", "ac")), 
		"find several matches");
	checkException(valChoice("a", c("ba", "ca")), 
		"finds no match");
	checkException(valChoice("A", c("ba", "ca")), 
		"matching is case-dependent");	
	checkEquals("ab",
		suppressWarnings(valChoice(c("a","b"), c("ba", "ab"))), 
		"multiple arg warns and selects only first one if 'several.ok' not set");
	
	checkEquals(c("ab", "ba"), 
		valChoice(c("a","b"), c("ba", "ab"), several.ok=TRUE), 
		"standard multiple match");
	checkEquals(c("ba", "ab"), valChoice(NULL, c("ba", "ab"), several.ok=TRUE), 
		"multiple match with arg=NULL");
	
	tf <- function(x=c("ab", "ba")) valChoice(x);
	checkEquals("ba", tf("b"), "take missing choices from calling formals");

	tf <- function(x) valChoice(x);
	checkException(tf("b"), 
		"fails if missing choices not found in calling formals");
	
	checkEquals("ab",
		valChoice(c("a","c"), c("ba", "ab"), several.ok=TRUE, 
			nomatch.action="rm", handler=NULL),
		"handler=NULL and nomatch.action='rm'");
	checkEquals(c("ab", NA_character_),
		valChoice(c("a", "c"), c("ba", "ab"), several.ok=TRUE, 
			nomatch.action="na", handler=NULL),
		"handler=NULL and nomatch.action='na'");
	
})



# Test function valArg()
test_valArg <- svTest(function() {
	checkEquals(c(FALSE, TRUE, FALSE, TRUE), 
		valArg(c(0,1), "logical", TRUE, 4, TRUE),
		"valArg with coercing and recycling");
})

# Test different default values and recycle combinations for function 
# valParamList()
test_valParamList_defaults <- svTest(function() {
	nams <- c("ab", "bc", "cd");
	checkEquals(list(ab=1, bc=3, cd=5),
		valParamList(list(a=1, c=5, 3), nams),
		"with partial matching and list default");

	checkEquals(list(ab=3, bc=3, cd=5),
		valParamList(list(c=5, 3), nams=nams, def=1),  
		"list default used when 'recycle' defaults to TRUE");

	checkEquals(list(ab=3, bc=1, cd=5),
		valParamList(list(c=5, 3), nams, def=1, recycle=FALSE), 
		"'def' used when recycle=FALSE");
	
	checkEquals(list(ab=1, bc=1, cd=5),
		valParamList(list(c=5), def=1, nams=nams),
		"'def' used when recycle=FALSE");
	
	checkEquals(list(ab=2, bc=2, cd=2),
		valParamList(list(2), nams, all.nams=TRUE),
		"a single unnamed element must be used as default, all.nams=TRUE");
	
	checkEquals(list(ab=1, bc=1, cd=1), valParamList(1, nams),
		"a single unnamed element must be used as default, all defaults");

	checkEquals(list(ab=1), 
		valParamList(1, nams, recycle=FALSE, all.nams=FALSE),
		"single unnamed element used as default, both recycle and all.nams FALSE");
	
})

# Test simplify = TRUE for valParamList()
test_valParamList_simplify <- svTest(function() {
	nams <- c("ab", "bc", "cd");
	checkEquals(structure(c(1, 3, 5), names=nams),
		valParamList(list(a=1, c=5, 3), nams, simplify=TRUE),  
		"simplify=TRUE returns a vector");

	checkEquals(list(ab=c(1, 2), bc=3, cd=5),
		valParamList(list(a=c(1, 2), c=5, 3), nams=nams, n=2, simplify=TRUE), 
		"simplify=TRUE is ignored if the result must be a list");

	checkEquals(structure(c(1, 3, 5), names=nams),
		valParamList(list(a=c(1, 2), c=5, 3), nams=nams, simplify=TRUE),  
		"simplify=TRUE is applied if n=1");	
})

# Test all.nams = FALSE for valParamList()
test_valParamList_notAllNams <- svTest(function() {
	nams <- c("ab", "bc", "cd");
	checkEquals(list(ab=1, bc=3, cd=5),
		valParamList(list(a=1, c=5, 3), nams, all.nams=FALSE), 
		"all.names=FALSE and all values provided when using list default");

	checkEquals(list(ab=1, cd=5),
		valParamList(list(a=1, c=5), nams, all.nams=FALSE), 
		"all.names=FALSE and not all values provided");

	checkEquals(structure(c(1, 5), names=c("ab", "cd")),
		valParamList(list(a=1, c=5), nams, def=100, all.nams=FALSE,  
			simplify=TRUE),
		"when all.names=FALSE and not all values provided, 'def' is ignored");
	
	checkEquals(list(bc=1),
		valParamList(list(bc=1), nams, recycle=TRUE, all.nams=FALSE),
		"if no unnamed default is provided, recycle makes nothing")
})

# Test what happens when some name of 'x' does not match any value in 'nams'
# for valParamList()
test_valParamList_noMatch <- svTest(function() {
	nams <- c("ab", "bc", "cd");
	checkException(valParamList(list(a=1, j=5, 3), nams),
		"names in 'x' that are not in 'nams' throw an error by default");
		
	checkEquals(list(ab=1, bc=3, cd=3),
		valParamList(list(a=1, j=5, 3), nams, nomatch.handler=NULL), 
		"names in 'x' not in 'nams' are ignored if nomatch.handler=NULL");
		
	produceWarning <- FALSE;
	tryCatch(valParamList(list(a=1, j=5, 3), nams, nomatch.handler="warning"), 
		error=function(e) 
			checkTrue(FALSE, "'warning' handler should not generate error"),
		message=function(e)
			checkTrue(FALSE, "'warning' handler should not generate message"),		
		warning=function(e) produceWarning <<- TRUE,
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='warning'"));
	checkTrue(produceWarning, "nomatch.handler='warning' must generate warning");
	
	x <- suppressWarnings(valParamList(list(a=1, j=5, 3), nams, 
			nomatch.handler="warning"));
	checkEquals(list(ab=1, bc=3, cd=3), x);
	
	produceMessage <- FALSE;
	tryCatch(valParamList(list(a=1, j=5, 3), nams, nomatch.handler="message"),
		error=function(e) 
			checkTrue(FALSE, "'message' handler should not generate error"),
		warning=function(e)
			checkTrue(FALSE, "'message' handler should not generate warning"),		
		message=function(e) produceMessage <<- TRUE,
		finally=checkTrue(TRUE, 
			"we should reach final block when handler='message'"));
	checkTrue(produceMessage, "nomatch.handler='message' must generate message");

	x <- suppressMessages(valParamList(list(a=1, j=5, 3), nams, 
			nomatch.handler="message"));
	checkEquals(list(ab=1, bc=3, cd=3), x);
	
})

relib <- function(pkg, ...) {
	if (paste("package", pkg, sep=":") %in% search())
		detach(paste("package", pkg, sep=":"));
	unloadNamespace(pkg);
	library(pkg, character.only=TRUE, ...);
}