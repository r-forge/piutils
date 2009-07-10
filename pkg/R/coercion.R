################################################################################
# R PACKAGE:   piUtils
# FILE:        R/coercion.R
# DESCRIPTION: Utilities related to coercion ("as") 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  29/06/2009
# LICENSE:     GPL-2
################################################################################

#' Try different methods of coertion until one works, or fail.
#' 
#' @title General Coercion 
#' @param x an \R object.
#' @param class class name to coerce to.
#' @param ... further arguments for the coercion function.
#' @param .params further arguments for the coercion function, as a list.
#' 
#' @return \code{x}, coerced to \code{class}.
#' @seealso \code{\link[methods]{as}} and \code{\link[methods]{canCoerce}}
#' 	in the \pkg{methods} package. 
#' @export

# TODO: Replace the try() calls with canCoerce(), see ?coerce

doCoerce <- function(x, class, ..., .params) {
	if (!is.character(class) || length(class) != 1L)
		stop(gettextf("%s must be a string (character vector of length 1)", 
				sqMsg(class)));
	dots <- list(...);
    if ((!missing(.params) && length(.params)) || length(dots)) {
		params <- if (missing(.params)) c(list(x), dots) 
			else c(list(x), .params, dots);
		
		tmp <- try(do.call(class, params), silent=TRUE);
		if (!inherits(tmp, "try-error")) return(tmp);
		
		tmp <- try(do.call(paste("as", class, sep="."), params), silent=TRUE);
		if (!inherits(tmp, "try-error")) return(tmp);				
	} else {
		isAlready <- if (isClass(class)) 
			tryCatch(is(x, class), error = inherits(x, class))
		else 
			tryCatch(match.fun(paste("is", class, sep="."))(x),
    			error = function(e) inherits(x, class));
		if (isAlready) return(x);
		
		tmp <- try(as(x, class), silent=TRUE);
		if (!inherits(tmp, "try-error")) return(tmp);
		
		tmp <- try(match.fun(paste("as", class, sep="."))(x), silent=TRUE);
		if (!inherits(tmp, "try-error")) return(tmp);

		tmp <- try(match.fun(class)(x), silent=TRUE);
		if (!inherits(tmp, "try-error")) return(tmp);		
	}
	stop(gettextf("cannot coerce %s to class %s", sqMsg("x"), dqMsg(class)));
}