################################################################################
# R PACKAGE:   piUtils
# FILE:        inst/unitTests/runit_messages.R
# DESCRIPTION: Test suite for messages functions.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  14/04/2009
# LICENSE:     GPL-2
################################################################################
# Run the suite simply by        example(unitTests.lfactor)
#      or set devPath() and      ut()
# Run individual tests with      (runTest(test_simplest_ltable()))

## Create a few objects we need for tests
x <- c("a", "b", "a"); 

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

# ------------------------------------------------------------------------------ 
# Test function nqMsg

test_nqMsg <- svTest(function() {
	checkEquals("a,b", nqMsg(x), "nqMsg with default parameters");		
	checkEquals("a,b,a", nqMsg(x, unique=FALSE), 
		"nqMsg with unique=TRUE");
	checkEquals("a/b", nqMsg(x, collapse="/"), 
		"nqMsg with collapse='/'");	
})

# ------------------------------------------------------------------------------ 
