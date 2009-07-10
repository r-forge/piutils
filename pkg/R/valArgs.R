################################################################################
# R PACKAGE:   piUtils
# FILE:        R/arg_validation.R
# DESCRIPTION: Utilities to validate function arguments.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  17/04/2009
# LICENSE:     GPL-2
################################################################################
# TODO: Allow to pass the number of calls up in the stack to the original
#		calling function, so the error message always uses it.

#' Test if a value is missing, empty, or contains only \code{NA} or \code{NULL} 
#' values.
#' 
#' @title Test Validity
#' @param x an \R object.
#' 
#' @return \code{TRUE} or \code{FALSE}.
#' @export
# NOTE: Copied from package gtool, then remade (is.atomic instead of is.vector,
#	is.recursive instead of is.list, and unify is.null with length(x) == 0),

invalid <- function(x) {
    if (missing(x) || length(x) == 0L) 
		return(TRUE);
	if (is.atomic(x))
		all(is.na(x))		
	else if (is.recursive(x))
		all(sapply(x, invalid))
    else FALSE;
}

#' Process parameters so that a list or vector of parameter specifications 
#' is returned.
#'  
#' This function receives a list or vector and creates a new named list or 
#' vector with names \code{nams} where:
#' \enumerate{ 
#'	\item named elements of \code{arg} are matched to \code{nams} using 
#' 		\link[base:pmatch]{partial matching}.
#'  \item subsequent unnamed elements of \code{arg} are assigned to unmatched 
#'		items in \code{nams} in order, recycling the values if 
#'		\code{recycle == TRUE}. If \code{all.nams == FALSE}, ends here.
#' 		Otherwise...	
#'  \item if \code{recycle == FALSE}, a default value \code{def} is 
#' 		provided, and there are still unmatched items in \code{nams}, 
#' 		assigns \code{def} to all those items.
#' }
#'  
#' This function can be used to more easily set manual parameters specification  
#' from the command line, for example to specify the color in a plot of a  
#' given named item to be different from all others.
#'  
#' This function is inspired on \code{\link[zoo]{make.par.list}} from 
#' package \pkg{\link[zoo]{zoo}} v1.5-4. 
#' 
#' @title Validate Parameter Specification Lists
#' @param arg list or vector of parameter specifications, see details.
#' @param nams character vector with the names of the parameters. 
#' @param n integer, number of rows. Each list element is recycled to this
#' 	length if specified, i.e. each list item in the result will have the
#'  same length \code{n}. Set it to \code{NULL} for no recycling of each
#'  element.
#' @param m integer. How is \code{arg} to be interpreted when it is not a list.
#'  If \code{m == 1}, the whole vector is taken as the first and only element 
#'  of a list (\code{list(x)}). 
#'  If \code{m > 1}, each element of \code{arg} turns into a list element 
#' 	(\code{as.list(x)}).    
#' @param def default parameter value. 
#' @param recycle logical. If \code{TRUE} recycle columns to provide 
#' 	unspecified ones. If \code{FALSE} use \code{def} to provide unspecified 
#' 	ones. This only applies to entire columns. 
#' 	Within columns recycling is always done regardless of how recycle is set. 
#' 	Defaults to \code{TRUE} if there is at least one unnamed variable and 
#' 	defaults to \code{FALSE} if there are only named variables in x.
#' @param all.nams logical flag. If \code{all.names == TRUE} (default), 
#'  requires that all names in \code{nams} are assigned a value in the 
#'  resulting list or vector. Otherwise, this constraint is not enforced and  
#'	the resulting object may have fewer elements than \code{nams}.
#' @param nomatch.handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the  
#'  argument does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#'   
#' @return A named list (if \code{simplify == FALSE} or each element has 
#'  more than one item) or named vector (if \code{simplify == TRUE} and each 
#'  element has one item) of parameters, sorted in the same order as 
#'  \code{nams}. See details.
#'  
#' @author Originally written by Achim Zeileis and Gabor Grothendieck, then
#'  modified for this package by Enrique Bengoechea. 
#' @seealso \code{\link[zoo:make.par.list]{make.par.list}} in package \pkg{zoo},  
#' 	and \code{\link{modifyList}} in \pkg{base}.
#' @export 

valParamList <- function(arg, nams, n=1, m=1, def, 
		recycle = any(unnamed) && all.nams, all.nams=TRUE, simplify=FALSE, 
		nomatch.handler="stop", .name, ...) {
    if (!is.list(arg)) 
        arg <- if (m == 1L) list(arg) else as.list(arg);
    y <- vector(mode="list", length=length(nams));
    names(y) <- nams;

    unnamed <- if (is.null(names(arg))) {
    	names(arg) <- rep("", length(arg));
		rep(TRUE, length(arg)) 
	} else is.na(names(arg)) | names(arg) == "";
	
    matchNams <- pmatch(names(arg), nams, nomatch=0L);
	inNams <- matchNams > 0L;
	noMatch <- !unnamed & !inNams;
	if (any(noMatch) && length(nomatch.handler) > 0L)
		.valCondition(sprintf(ngettext(sum(noMatch),
			"element %s not found in %s",
			"elements %s not found in %s"),
			dqMsg(names(arg)[noMatch]), sqMsg("nams")), 
			.argName(.name), handler=nomatch.handler, ...);
	
    if (all.nams && (!recycle || !any(unnamed))) {
    	if (missing(def))
			.valCondition(gettextf(
				"%s is required when all.nams=TRUE and no default is provided in %s",
				sqMsg("def"), sqMsg(.argName(.name))), handler="stop", ...)
		else if (is.null(def)) def <- list(NULL);
        y[] <- def;		
	}
    y[matchNams[inNams]] <- arg[inNams];
	
    if (recycle && any(unnamed)) {
        y[!seq_along(y) %in% matchNams] <- arg[unnamed];
    } else {
    	unassigned <- which(!seq_along(y) %in% matchNams); 
		y[unassigned[seq_len(sum(unnamed))]] <- arg[unnamed];
		if (!all.nams && length(unassigned) - sum(unnamed) > 0L)
			y[unassigned[seq(from=sum(unnamed)+1, to=length(unassigned))]] <- 
				NULL;  		
	}

    sapply(y, function(y) 
			if (length(y) == 1) y else rep(y, length.out = n), 
		simplify=simplify);
}


#' Generates a meaningful message for validation functions, showing
#' whenever possible the name of the original function that invoked the
#' validation.
#' 
#' This is an internal function to be used exclusively by other validation 
#' functions in this package. It should never be directly invoked from the R 
#' command line.
#' 
#' @title Executes the Validation Condition Handler
#' @param msg string with the message to show.
#' @param handler function or function name specifying the condition
#'  handler to call.
#'  This is usually one of \code{stop}, \code{warning} or \code{message},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept \code{call.} as argument. \cr
#'  The default handler is the one defined in argument \code{handler}
#'  in the funcion inmediately up in the call stack.
#'  If the handler has zero-length (e.g. \code{NULL}), makes nothing. 
#' @param ... further arguments to pass to the \code{handler} function.
#' 
#' @return Whatever the \code{handler} function returns. Nevertheless, this
#' 	value is usually ignored in the calling function.
#' @note This function was originally inspired on the \code{onExit} inner
#'  function of \code{\link[fda]{checkLogical}} in package \pkg{fda}. 
#  NOT EXPORTED

.valCondition <- function(msg, 
		handler=get("handler", sys.frame(-1), inherits=FALSE), ...) {
	# Gets the name of the original function invoked, to make the message
    # more informative. It handles both cases where the argument validation 
	# function (that is one step up in the call stack) has been invoked from 
    # another function, or directly from the command line.		
	callEnv <- sys.call(-2)[[1]]
    if (is.null(callEnv)) 
        callEnv <- sys.call(-1)[[1]]
		
	msg <- paste(gettextf("in %s: ", as.character(callEnv)), msg, sep="");
	if (length(handler) > 0L)
		match.fun(handler)(msg, call.=FALSE, ...);
}	

#' This function is intended to provide a meaningful name for the original 
#' argument when invoked from argument validation functions, but returns 
#' something even when the parent function in the call stack has been called 
#' from the R command line, and defaults to just \code{"arg"} if there isn't 
#' something better.
#' 
#' This is an internal function to be used exclusively by other validation 
#' functions in this package. It should never be directly invoked from the R 
#' command line.
#' 
#' @title Get Meaningful Argument Names
#' @param .name string with the argument name, in the case it is explicitly
#'  provided, what happens only when one validation function invokes some
#'  other validation functions.
#' @param max.char integer. Maximun number or characters in the name.
#'  This is useful when \code{.name} ends up being a deparsed long expression. 
#' 
#' @return A string with an argument name. 
#  NOT EXPORTED

.argName <- function(.name, max.char=30) {
	if (missing(.name) || length(.name) == 0L) {
		.name <- if (is.name(substitute(arg, parent.frame(1)))) {
			if (substitute(arg, parent.frame(1)) == "") "arg" 
			else shorten(deparse(substitute(arg, parent.frame(1))), max.char)
		} else "arg";
	} else shorten(.name, max.char);
}

#' Ensures that a function argument satisfies the supplied class specification,
#' optionally coercing to the target class. 
#' 
#' @title Validate Function Arguments Class
#' @param arg the argument (any \R object).
#' @param class string. Target class of the argument.
#' @param coerce logical flag or list. Whether to try to coerce \code{arg} 
#'  to the target \code{class} if it is not an object of that class. If it
#'  is a list, their elements are used as parameters to the coercion function.
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the argument 
#'  does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' @param ... Further arguments to pass to function \code{handler}.
#' 
#' @return \code{arg}, possibly modified to comply with the validation
#'  requirements. As a side effect, \code{handler} is executed
#'  if the argument does not meet the validation criteria, which defaults
#'  to raising an error via \code{stop}.
#' @seealso \code{\link{valLength}}
#' @export

valClass <- function (arg, class, coerce=FALSE, handler="stop", 
		.name, ...) {	
#	if (missing(arg)) 
#		.valAction(gettextf("%s is missing, but it is required", 
#			sqMsg(.argName(.name))), handler="stop", ...);	
	if (is.null(arg)) 
		.valCondition(gettextf("%s is NULL, but NULL is not allowed", 
			.argName(.name)), ...);
		
	if (!is(arg, class)) {
		if (identical(coerce, FALSE)) 
	        .valCondition(gettextf("class(%s) = %s, but should be %s", 
				.argName(.name), sqMsg(class(arg)), sqMsg(class)), ...)
		else if (identical(coerce, TRUE))
			arg <- doCoerce(arg, class)
		else if (is.list(coerce))
			arg <- doCoerce(arg, class, .params=coerce)
		else stop("%s argument is not valid", sqMsg("coerce"));
	}
    
    arg;
}

#' Ensures that a function argument satisfies the supplied length 
#' specification, possibly for each dimension, and optionally recycling it to 
#' the target length. 
#' 
#' Lengths can be specified using integer vectors of possible valid lengths. 
#' \code{NA} has the special meaning of "any positive length but not zero".
#'  
#' For recursive objects, the length specification can be a list of integer  
#' vectors giving the target length for each dimension. In this case, no
#' recycling is possible. 
#' 
#' @title Validate Function Arguments Length
#' @param arg the argument (any \R object).
#' @param len an integer vector or list of integer vectors with the 
#' 	specification of accepted lengths.
#' 	Each integer vector gives the set of accepted lengths (for the 
#' 	corresponding dimension when \code{len} is a list), including 0 if  
#'  zero-length objects should be accepted. The value \code{NA} is equivalent
#'  to "any positive length but not zero".
#' @param recycle logical flag; if \code{TRUE} and the object length does not
#' 	match any of the specified lenghts, its value (provided it has non-zero
#'  length) is recycled (reduced or increased) to the first non-zero length 
#'  in \code{len}, and a warning is issued.
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the argument 
#'  does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' @param ... Further arguments to pass to function \code{handler}.
#'
#' @return \code{arg}, possibly recycled to a new length. As a side effect, 
#'  \code{handler} is executed if the argument does not meet the 
#'  validation criteria, which defaults to raising an error via \code{stop}.
#' 
#' @seealso \code{\link{valClass}}.
#' @export
# TODO: Instead of using a list spec for 'len', why not have a valDim function? 

valLength <- function(arg, len=NA_integer_, recycle=FALSE, 
		handler="stop", .name, ...) {
	xLength <- length(arg);	
	len <- as.integer(len);
	
	if (all(is.na(len))) {
    	if (xLength == 0L) 
			.valCondition(gettextf("length(%s) == 0, but must be non-zero",
				.argName(.name)), ...)
	} else if (!xLength %in% len) {
		if (xLength > 0L && recycle) {
			len <- len[len > 0L][1L];
			if (len %% xLength != 0L) 
    			.valCondition(gettextf(
					"target length %i is not a multiple of length(%s) = %i",
					len, .argName(.name), xLength), handler="warning", ...);			
			arg <- rep(arg, length.out=len);
		} else .valCondition(sprintf(ngettext(length(len),
			"length(%s) = %i, but must be %s",
			"length(%s) = %i, but must be any of %s"),
			.argName(.name), xLength, nqMsg(len, collapse=" / ")), ...);
	}
	
	arg;	
}

#' Ensures that a function argument satisfies the supplied dimensions  
#' specification, verifying the number of dimensions and, optionally, the
#' length for each dimension. 
#' 
#' @title Validate Function Arguments Dimensions
#' @param arg the argument (any \R object).
#' @param dims an integer vector or list of integer vectors with the 
#' 	specification of accepted lengths for each dimension.
#'  If it is an integer vector, the number of dimensions of \code{arg} must
#'  be in \code{dims}, and \code{dims = NA} means "any positive length but not 
#'  zero". \cr
#'  If \code{dims} is a list, each component gives the set of accepted lengths  
#' 	for each dimension.
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the argument 
#'  does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' @param ... Further arguments to pass to function \code{handler}.
#'
#' @return \code{arg}. As a side effect, 
#'  \code{handler} is executed if the argument does not meet the 
#'  validation criteria, which defaults to raising an error via \code{stop}.
#' 
#' @seealso \code{\link{valLength}}.
#' @export

valDim <- function(arg, dims=list(), handler="stop", .name, ...) {
	stop("not yet implemented");
#	xLength <- length(x);
#	xDim <- dim(x);
#	targetDims <- if (!is.list(dims)) as.integer(dims) else length(dims);
#	
#	if ()
#	if (is.list(dims)) {
#		xDim <- dim(x);
#		if (is.null(xDim)) 
#			.valAction(gettextf(
#				"%s has no dimensions, but an object of dimension %i is required", 
#				.argName(.name), xDim), handler="stop", ...);
#		if (length(xDim) != length(dims)) {
#			.valAction(gettextf("dim(%s) = %d, but %i dimensions are required",
#				.argName(.name), xLength, length(xDim)));
#		} else {
#			dims <- lapply(dims, as.integer);
#			for (i in seq_along(dims))
#				if (!xDim[i] %in% dims[[i]])
#					.valAction(sprintf(ngettext(length(dims[[i]]),
#						"length(%s) == %i on dimension %i, but must be %i",
#						"length(%s) == %i on dimension %i, but must be any of %i"),
#						.argName(.name), xDim[i], i, 
#						nqMsg(dims[[i]], collapse=" / ")));
#		}
#	}
}

valNA <- function(arg, some.action=NULL, some.handler="stop",
		all.action=some.action, all.handler=some.handler, .name, ...) {
	if (length(arg) == 0L) return(arg);
	
	isNA <- is.na(arg);	
	if (all(isNA)) {
		if (length(all.handler) > 0L)
			.valCondition(gettextf("%s only has NA values", .argName(.name)), 
				handler=all.handler, ...);		
		if (length(all.action) > 0L) 
			arg <- match.fun(all.action)(arg);
	} else if (any(isNA)) {
		if (length(some.handler) > 0L)
			.valCondition(sprintf(ngettext(sum(isNA),
				"%s has %i NA value",
				"%s has %i NA values"),
				.argName(.name), sum(isNA)), handler=some.handler, ...);		
		if (length(some.action) > 0L) 
			arg <- match.fun(some.action)(arg);	
	}
	
	arg;
}

#' Matches \code{arg} against a table of candidate values specified by 
#' \code{choices}, where \code{NULL} means to take the first one.
#' This is normally used for function arguments validation. 
#'
#' Matching is done using \code{\link{pmatch}}, so \code{arg} may be 
#' abbreviated.
#'  
#' In the one-argument form \code{valChoice(arg)}, the choices are obtained 
#' from a default setting for the formal argument \code{arg} of the function 
#' from which \code{valChoice} was called, exactly as \code{match.arg} does.
#'  
#' \code{valChoice} differs from \code{\link[base]{match.arg}} in several 
#' ways:
#' \enumerate{
#'  \item rather than failing when \code{arg} or \code{choices} are not 
#'   	character vectors, they are coerced to character.
#'  \item improved error messages, using the original argument name from 
#' 		the invoking function if possible, and specifying the exact values that 
#'  	do not match.
#'  \item option to stop or warn if some value is not matched when 
#'  	\code{several.ok = TRUE}, while \code{match.arg} silently removes the 
#' 		value in this case. 
#' }
#' @title Validate Arguments against Choices
#' @param arg a vector (of length one unless \code{several.ok} is \code{TRUE}) 
#' 	or \code{NULL}. It is coerced to character.
#' @param choices a vector of candidate values. It is coerced to character.
#'  If not supplied, it is inferred from the default value of the corresponding
#' 	argument in the formals of the calling function. Clearly, this fails if
#'  invoked from the command line, so whenever called directly \code{choices}
#'  must be supplied.    
#' @param several.ok logical specifying if \code{arg} is allowed to have more 
#' 	than one element.
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the argument 
#'  does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' 
#' @return The unabbreviated version of the exact or unique partial match if 
#'  there is one; otherwise, an error is signalled if \code{several.ok} is 
#'  false, as per default. When \code{several.ok} is true and more than one 
#'  element of \code{arg} has a match, executes \code{nomatch.action} if 
#'  some element does not match, and, if applicable, returns all unabbreviated 
#'  versions of matches.
#' @seealso \code{\link{match.arg}}.
#' @export
# TODO: Allow to parameterize the match type (partial, exact, case sensitive).
# TODO: Write valChoice unit tests

valChoice <- function(arg, choices, several.ok=FALSE, 
		nomatch.action=c("rm", "na"), handler="stop", .name, ...) {
	if (missing(choices) || is.null(choices)) {
		# A more descriptive error message could be generated if  
		# the choice table is not found, but because it will be found 
    	# at development time, it does not seem worthwhile the extra time
		# to check.
        formal.args <- formals(sys.function(sys.parent()));
        choices <- eval(formal.args[[deparse(substitute(arg))]]);
	}    	
   	if (length(arg) == 0L || identical(arg, choices)) 
    	return(if (several.ok) choices else choices[1L]);

	if (!several.ok && length(arg) > 1L) {
		.valCondition(gettextf("length(%s) = %i, but must be %i", 
			sqMsg(.argName(.name)), length(arg), 1L), 
			handler="warning", ...);
		arg <- arg[1L];
	} 

    i <- pmatch(as.character(arg), as.character(choices), nomatch=0L, 
		duplicates.ok=TRUE);		
	if (any(noMatch <- (i == 0L))) { 
		.valCondition(sprintf(ngettext(sum(noMatch),
			"value %s of %s is not a valid choice in %s", 
			"values %s of %s are not valid choices in %s"),
			dqMsg(arg[noMatch]), sqMsg(.argName(.name)), 
			dqMsg(choices, max=5)), ...);
		switch(match.arg(nomatch.action),
			na = i[noMatch] <- NA_integer_, 
	    	i <- i[!noMatch]);	
	} 
	
    choices[i];	
}

#' A wrapper to \code{\link{valChoice}} that extracts the choices from 
#' the names of an object.
#' 
#' @title Validate Arguments against Object Names
#' @param arg a vector.
#' @param object \R object whose names are to be used as choices. 
#' @param .name string to use for \code{arg} on error messages.
#' @param ... arguments passed through to \code{valChoice}.
#' 
#' @return The unabbreviated version of the exact or unique partial matches,
#'  as returned by \code{valChoice}.
#' @seealso \code{\link{valChoice}}.
#' @export

valNames <- function(arg, object, .name, ...) 
	valChoice(arg, names(object), .name=.argName(.name), ...)	


#' @title Validate Function Arguments 
#' @param x an \R object.
#' 
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the argument 
#'  does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' @param ... Further arguments to pass to function \code{handler}.
#'
#' @return \code{arg}, possibly recycled to a new length. As a side effect, 
#'  \code{handler} is executed if the argument does not meet the 
#'  validation criteria, which defaults to raising an error via \code{stop}.
#' @seealso \code{\link{valClass}}, \code{\link{valLength}}, 
#' 	and \code{\link{valChoice}}, \code{\link{valNames}} and 
#'  \code{\link{match.arg}}.
#' @export

valArg <- function(arg, class, coerce=FALSE, len, recycle=FALSE, null.ok=FALSE,   
		choices, handler="stop", ...) {
	.name <- .argName();		
#	if (required && missing(arg)) 
#		.valAction(gettextf("%s is missing, but it is required", 
#			sqMsg(.argName(.name))), handler="stop", ...);	
	if (null.ok) 
		if (is.null(arg)) return(NULL)
	else if (is.null(arg)) 
		.valCondition(gettextf("%s is NULL, but NULL is not allowed", 
			.argName(.name)), ...);

	if (!missing(class)) 
		arg <- valClass(arg, class=class, coerce=coerce, 
			handler=handler, .name=.name, ...);

#	if (!na && any(is.na(x)))
#		stop(gettextf("%s cannot contain NAs", sqMsg(argName)));
	
	if (!missing(choices))
		arg <- match.choices(arg, choices=choices, as.factor=as.factor, 
			drop.levels=drop.levels, .argName=argName);

	if (!missing(len)) {
		if (is.list(len))
			arg <- valDim(arg, dims=len, handler=handler, 
				.name=.name, ...)
		else
			arg <- valLength(arg, len=len, recycle=recycle,  
				handler=handler, .name=.name, ...);
	}
	
	arg;
}	

