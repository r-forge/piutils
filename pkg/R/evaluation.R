################################################################################
# R PACKAGE:   piUtils
# FILE:        R/evaluation.R
# DESCRIPTION: Utilities related to evaluation of R code. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  29/06/2009
# LICENSE:     GPL-2
################################################################################
# TODO: Validate parameters as much as possible for each method:
#		- For calls and expressions, that named parameters appear in
#		  the R expression. For unnamed parameters, assign sequentially to
#		  used variables.
#  		- For functions, that their formals (can) match the parameters.

#' Provides a common interface to evaluate \R objects such as functions,
#' names, expressions or calls. 
#' 
#' One of the best ways to achieve flexibility in \R programs is to 
#' allow users to provide arbitrary \R code to perform a given functionallity. 
#' Thus, allowing object attributes, slots, or function arguments to accept
#' either data or language objects (such as expressions, names, or 
#' calls) that return the data objects when evaluated, is one of
#' the best ways to allow programs to evolve in unforeseen ways.
#' 
#' \code{doEval} is an \link[base:UseMethod]{S3 generic} that provides
#' a common interface to the evaluation mechanism of different \R language 
#' objects such as \code{\link[base]{eval}}, \code{\link[base]{do.call}} or
#' \code{\link[base]{match.fun}}.
#' 
#' Using this generic, the developer allows users to
#' replace direct values in their function invocations by functions, function 
#' names, or (\link[base:substitute]{quoted}) expression or names, with.
#' In addition, by making use of S3 inheritance, new classes
#' of evaluable objects can be supplied in the future without modifying
#' existing code simply by adding a corresponding method to \code{doEval}.  
#' 
#' Argument names for this methods all start by a dot to minimize conflicts
#' with names in \dots. Also beware that partial matching of argument names
#' does not apply to those arguments after \dots (\code{.params} and
#' \code{use.names}).
#' 
#' \section{Methods}{
#' The default method returns its argument unmodified. This is provided so
#' that objects that cannot be evaluated by a specific method (and thus
#' are not "evaluable") are returned unmodified.
#' 
#' Other methods are provided for the following classes:
#' \describe{
#' 	\item{\coded{call} and \code{expression}}{normally the result of quoting 
#' 		an \R expression. These are evaluated in an environment that has as 
#' 		variables the parameters passed to \code{doEval}.}
#' 	\item{\code{name}}{If the name references a function object, evaluation 
#' 		is passed to \code{doEval.function}, otherwise is is evaluated
#' 		in the same way as \code{call}s and \code{expression}s.}
#' 	\item{function}{the function is invoked using as arguments the parameters
#' 		passed in \dots or \code{.params}.}
#' 	\item{character}{only the first element is used. If it is a string
#' 		that supplies a valid function (as a result of invoking 
#' 		\code{\link[base]{match.fun}}), the function is invoked in the
#' 		same way as the previous case. Otherwise, an error is raised.}
#' }
#' }
#' 
#' @title A Common Interface to Evaluate R Objects
#' @aliases doEval.default doEval.call doEval.name doEval.expression
#' 	doEval.function doEval.character is.evaluable
#' @usage 
#'  doEval(.object, \dots)
#' 
#'  \method{doEval}{default}(.object, \dots, .params, .use.names = TRUE)
#'  \method{doEval}{call}(.object, \dots, .params, .use.names = TRUE)
#'  \method{doEval}{name}(.object, \dots, .params, .use.names = TRUE)
#'  \method{doEval}{expression}(.object, \dots, .params, .use.names = TRUE)
#'  \method{doEval}{function}(.object, \dots, .params, .use.names = FALSE)
#'  \method{doEval}{character}(.object, \dots, .params, .use.names = FALSE)
#' 
#' @param .object an \R object.
#' @param ... additional parameters to the evaluation. These can be
#' 	
#' @param .params list with additional parameters to the evaluation. This
#' 	is convenient when the parameters are already in list form in client
#' 	code. If both \code{.params} and \dots are specified, they are joined
#' 	together in a single list of parameters where \dots values take 
#' 	precedence over \code{.params} values in the case of name overlaps. 
#' @params .use.names logical flag. If \code{.object} is a function, whether
#' 	arguments should be matched by name (when \code{.use.names = TRUE})
#'  or by order. See details.
#' @params x in \code{is.evaluable}, any \R object.
#' 
#' @return For \code{doEval}, the result of evaluating \code{.object} if it  
#' 	can be evaluated, or \code{.object} unchanged if it cannot.
#' 
#'  For \code{is.evaluable}, \code{TRUE} if \code{.object} can be evaluated,
#'  or \code{FALSE} otherwise. Evaluable objects are considered to be
#'  language objects (\code{name}s, \code{call}s or \code{expression}s),
#'  functions, or strings that define valid function names.
#'    
#' @seealso \code{\link[base]{eval}}, \code{\link[base]{do.call}},
#'  \code{\link[base]{match.fun}}, \code{\link[base]{is.language}}.  
#' @export
#  TODO: Document the logic for using .use.names=FALSE

doEval <- function(.object, ...) UseMethod("doEval");

#' @nord
#' @S3method doEval default

doEval.default <- function(.object, ...) .object;

#' @nord
#' @S3method doEval default
# .use.names ignored in doEval.call, included for API compatibility with  
# other methods

doEval.call <- function(.object, ..., .params, .use.names=TRUE) {
	if (missing(.params) || !length(.params))
		eval(.object, list(...))
	else eval(.object, .params)
}

#' @nord
#' @S3method doEval default

doEval.name <- function(.object, ..., .params, .use.names=TRUE) {
	result <- doEval.call(.object, ..., .params=.params, .use.names=.use.names);
	if (is.function(result))
		doEval.function(result, ..., .params=.params, .use.names=.use.names)
	else result;
}

#' @nord
#' @S3method doEval default

doEval.expression <- function(.object, ..., .params, .use.names=TRUE) 
	doEval.call(.object, ..., .params=.params, .use.names=.use.names);

#' @nord
#' @S3method doEval default

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

#' @nord
#' @S3method doEval default

doEval.character <- function(.object, ..., .params, .use.names=FALSE) 
	doEval.function(match.fun(.object), ..., .params=.params, 
		.use.names=.use.names);

#' @nord
#' @export

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