################################################################################
# R PACKAGE:   piUtils
# FILE:        R/evaluation.R
# DESCRIPTION: Utilities related to evaluation of R code. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  29/06/2009
# LICENSE:     GPL-2
################################################################################

doEval <- function(.object, ...) UseMethod("doEval");

doEval.default <- function(.object, ...) .object;

# .use.names ignored in doEval.call, included for API compatibility with other 
# methods
doEval.call <- function(.object, ..., .params, .use.names=TRUE) {
	if (missing(.params) || !length(.params))
		eval(.object, list(...))
	else eval(.object, .params)
}

doEval.name <- function(.object, ..., .params, .use.names=TRUE) {
	result <- doEval.call(.object, ..., .params=.params, .use.names=.use.names);
	if (is.function(result))
		doEval.function(result, ..., .params=.params, .use.names=.use.names)
	else result;
}

doEval.expression <- function(.object, ..., .params, .use.names=TRUE) 
	doEval.call(.object, ..., .params=.params, .use.names=.use.names);

doEval.function <- function(.object, ..., .params, .use.names=FALSE) {
	hasParams <- (!missing(.params) && length(.params)); 
	if (!hasParams && .use.names) 
		return(.object(...));
    dots <- if (hasParams) 
		modifyList(list(...), .params) 
	else 
		list(...);    
	if (!.use.names) 
		names(dots) <- NULL;
	
	do.call(.object, dots);
}

doEval.character <- function(.object, ..., .params, .use.names=FALSE) 
	doEval.function(match.fun(.object), ..., .params=.params, 
		.use.names=.use.names);

is.evaluable <- function(x) {
	if (is.function(x) || is.language(x))
		return(TRUE)
	else if (is.character(x) && length(x) == 1L && !is.na(x)) {
		result <- TRUE;
		tryCatch(match.fun(x), 
			warning=function(w) NULL,
			error=function(e) result <<- FALSE);
		result;
	} else FALSE;
}