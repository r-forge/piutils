################################################################################
# R PACKAGE:   piUtils
# FILE:        R/valIndex.R
# DESCRIPTION: Utilities to validate object indices. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  22/06/2009
# LICENSE:     GPL-2
################################################################################

#' Validates object indices ("subscripts") used in extraction or replacement
#' functions, ensuring that they specify elements within the bounds of the 
#' object.
#' 
#' Default subsetting operators such as \code{\link[base:Extract]{[}} usually 
#' return \code{NA}s or \code{NULL}s when requesting indices beyond the object
#' lengths, and these cannot be discriminated afterwards from real \code{NA}s
#' or \code{NULL}s in the data. This is often inconvenient and raising a 
#' \link[base:conditions]{condition} (such as an error, warning or message) may  
#' be preferred, which can be done using this function before extracting or 
#' replacing. 
#' 
#' It is also common to create new functions with some argument that will be 
#' used to index an object. Although the index can be passed directly to 
#' an indexing operator, it is often useful to be able to validate it before
#' or afterwards, or to express it in one canonical form (e.g. as a character
#' vector) for doing some computations with it. This is another common
#' use case for \code{valIndex}.
#' 
#' \code{valIndex} is a \link[base:UseMethod]{generic function}, with methods
#' for \code{logical}, \code{numeric} and \code{character}. Packages and users
#' can add new methods. This is also helpful if you plan to create an object
#' that can be indexed in the standard way, but that may need to use a 
#' different type of index (such as a date class for time series). 
#' Using \code{valIndex} and adding a method for the additional index class
#' achieves this effect with minimal coding.
#'  
#' @title Validate Object Indices
#' @aliases valIndex.default valIndex.logical valIndex.numeric 
#' 	valIndex.character
#' @usage 
#' 	valIndex(arg, object, ...)  
#' 
#' 	\method{valIndex}{logical}(arg, object, dim, retclass, several.ok = TRUE, 
#' 		mark.new = FALSE, handler = "stop", .name, \dots)
#' 	\method{valIndex}{numeric}(arg, object, dim, retclass, several.ok = TRUE, 
#' 		mark.new = FALSE, handler = "stop", .name, \dots)
#' 	\method{valIndex}{character}(arg, object, dim, retclass, several.ok = TRUE, 
#' 		mark.new = FALSE, handler = "stop", .name, \dots)
#' 
#' @param arg index specifying elements to extract or replace from 
#' 	\code{object}. Can be a logical, integer, or character vector, as in 
#'  \link[base:Extract]{[-indexing}. 
#' @param object \R object to be indexed by \code{arg}.
#' @param dim integer with the dimension to use when indexing \code{object}.
#'  Required only when \code{object} has dimensions and you want \code{arg}
#'  to work only on one specific dimension.
#' @param retclass string. One of \code{"logical"}, \code{"integer"} or
#'  \code{"character"}. If specified, determines the class of the returned 
#'  index, which is transformed if necessary. Negative integer values are
#'  transformed to positive integer values when \code{retclass == "integer"}. 	
#' @param several.ok logical specifying if \code{arg} is allowed to have more 
#' 	than one element. If only one element is allowed and the handler does not
#' 	stop execution, \code{arg} is modified to ensure that only the first
#' 	selected element is returned.
#' @param handler function or function name specifying the 
#'  \code{\link[base:conditions]{condition}} handler to execute when the  
#'  argument does not validate.
#'  This is usually one of \code{"stop"}, \code{"warning"} or \code{"message"},
#'  but can be any function that has as first argument the message to show,
#'  and must also accept argument \code{call.}
#' @param mark.new logical flag. If \code{mark.new = TRUE} and the handler does
#'  not stop execution, the vector returned includes an attribute 
#'  \code{is.new} of the same length as \code{arg} that indicates whether
#'  each element would index an existing element of \code{object} (when 
#'  \code{FALSE}) or a new one (when \code{TRUE}). When \code{mark.new = TRUE},
#'  negative numeric indexes are trasformed to their equivalent positive values.  
#' @param .name string with the name to use on error messages instead of
#'  just \code{"arg"}. If it is \code{NULL} and \code{arg} in the invoking 
#'  function is a name, uses the name, otherwise defaults to \code{"arg"}.
#'  This is to be used only when \code{valChoice} is invoked from another
#'  validation function, to generate more descriptive messages.   
#' @param ... Further arguments to pass to function \code{handler}. 
#' 
#' @return \code{arg}. As a side effect, \code{handler} is executed whenever
#'  \code{arg} would not be a valid index for \code{object}, for example
#'  when it would fall out of bounds. 
#' @export

valIndex <- function(arg, object, ...) UseMethod("valIndex")

#' @nord
#' @S3method valIndex default

valIndex.default <- function(arg, object, ...) {
	if (is.null(arg))
		logical(0L)
	else 
		stop(gettextf("no applicable method to index by class %s", 
			dqMsg(class(arg), collapse=" > ")));
}

#' @nord
#' @S3method valIndex logical

valIndex.logical <- function(arg, object, dim, retclass, several.ok=TRUE, 
		mark.new=FALSE, handler="stop", .name, ...) {
	argLen <- length(arg);	
	if (argLen == 0L) return(logical(0L));	
	objLen <- if (missing(dim)) length(object) 
		else dim(object)[as.integer(dim)[1L]];	
	
	if (argLen <= objLen) {
		if (argLen < objLen) {
			if (objLen %% argLen != 0L)  
    			.valCondition(gettextf(
					"object length %i is not a multiple of index %s length = %i",
					objLen, sqMsg(.argName(.name)), argLen), 
					handler="warning", ...);			 					
  			arg <- rep(arg, length.out=objLen);
		}
		isNew <- rep(FALSE, argLen);
	} else if (argLen > objLen) { 
		.valCondition(gettextf(
			"index %s length %i is greater than object length %i",
			sqMsg(.argName(.name)), argLen, objLen), ...);
		isNew <- c(rep(FALSE, objLen), rep(TRUE, argLen-objLen));
	}
	
	if (!several.ok && sum(arg) > 1L) {
    	.valCondition(sprintf(ngettext(sum(arg),
			"index %s selects %i element, but must select only 1",
			"index %s selects %i elements, but must select only 1"),
			sqMsg(.argName(.name)), sum(arg)), ...);
		arg[which(arg)[-1L]] <- FALSE; 	
	}
		
	if (!missing(retclass)) {
		retclass <- match.arg(retclass, c("logical", "integer", "character"));

		if (retclass == "character") {
			objNames <- if (missing(dim)) names(object) 
				else dimnames(object)[[as.integer(dim)[1]]];
	    	if (is.null(objNames))
				.valCondition(gettextf(
					"cannot return a character index for %s with an unnamed object", 
					sqMsg(.argName(.name))), handler="stop", ...);
			if (mark.new) isNew <- isNew[arg];
			arg <- objNames[arg];
		} else if (retclass == "integer") {
    		if (mark.new) isNew <- isNew[arg];
			arg <- which(arg);
		}
	} 
	
	if (mark.new) attr(arg, "is.new") <- isNew; 	
	arg;		
}

#' @nord
#' @S3method valIndex numeric

valIndex.numeric <- function(arg, object, dim, retclass, several.ok=TRUE, 
		mark.new=FALSE, handler="stop", .name, ...) {
	if (length(arg) == 0L) return(integer(0L));	
	objLen <- if (missing(dim)) length(object) 
		else dim(object)[as.integer(dim)[1L]];	

	arg <- as.integer(arg);
	isPositive <- any(arg >= 0L);
	if (isPositive && any(arg < 0L))
    	.valCondition(gettextf(
			"index %s cannot mix positive and negative subscripts",
			sqMsg(.argName(.name))), handler="stop", ...);			
	
	if (isPositive) {
		isNew <- rep(FALSE, length(arg));
    	if (any(isGreater <- arg > objLen)) {
			.valCondition(sprintf(ngettext(sum(isGreater),
				"index %s value %s is greater than object length %i",
				"index %s values %s are greater than object length %i"), 
				sqMsg(.argName(.name)), nqMsg(arg[isGreater]), objLen), ...);			
			isNew[isGreater] <- TRUE;
		}
	} else {
		asPosIndex <- seq_len(objLen)[arg];
		if (mark.new) { 
			arg <- asPosIndex;			
			isNew <- rep(FALSE, length(arg));
			isPositive <- TRUE;
		}
	}

	if (!several.ok) {
		if (isPositive) {
    		if (length(arg) > 1L) {
		    	.valCondition(sprintf(ngettext(sum(arg),
					"index %s selects %i element, but must select only 1",
					"index %s selects %i elements, but must select only 1"),
					sqMsg(.argName(.name)), sum(arg)), ...);
				arg <- arg[1L];
				if (mark.new) isNew <- isNew[1L];
			}
		} else {			
			nSelItems <- length(asPosIndex); 
			if (nSelItems > 1L) {
		    	.valCondition(sprintf(ngettext(nSelItems,
					"index %s selects %i element, but must select only 1",
					"index %s selects %i elements, but must select only 1"),
					sqMsg(.argName(.name)), nSelItems), ...);
				arg <- asPosIndex[1L];
			}
		}
	}
	
	if (!missing(retclass)) {
		retclass <- match.arg(retclass, c("logical", "integer", "character"));
		
		if (retclass == "character") { 
			objNames <- if (missing(dim)) names(object) 
				else dimnames(object)[[as.integer(dim)[1]]];
	    	if (is.null(objNames))
				.valCondition(gettextf(
					"cannot return a character index for %s with an unnamed object", 
					sqMsg(.argName(.name))), handler="stop", ...);		    		
			arg <- objNames[arg];			
		} else if (retclass == "logical") {
			nLen <- max(objLen, max(arg, na.rm=TRUE));
			if (mark.new && isPositive) { 
				isNew <- rep(FALSE, nLen);
				if (length(isNew) > objLen)
					isNew[(objLen+1):length(isNew)] <- TRUE;
			}
			seqLen <- seq_len(nLen);
			arg <- seqLen %in% seqLen[arg];	# Work for both indexes > and < 0 
		} else if (retclass == "integer" && !isPositive)
			arg <- asPosIndex; 			
	}
	
	if (mark.new) attr(arg, "is.new") <- isNew;
	arg;
}

#' @nord
#' @S3method valIndex character

valIndex.character <- function(arg, object, dim, retclass, several.ok=TRUE, 
		mark.new=FALSE, exact=FALSE, handler="stop", .name, ...) {
	argLen <- length(arg);	
	if (argLen == 0L) return(character(0L));	
	objLen <- if (missing(dim)) length(object) 
		else dim(object)[as.integer(dim)[1L]];	

	objNames <- if (missing(dim)) names(object) 
			else dimnames(object)[[as.integer(dim)[1]]];
	if (is.null(objNames)) 
		.valCondition(gettextf(
			"cannot use character index %s with an unnamed object", 
			sqMsg(.argName(.name))), handler="stop", ...)
	
	i <- if (exact)
		match(as.character(arg), objNames, nomatch=0L)	    		
	else 
		pmatch(as.character(arg), objNames, nomatch=0L, duplicates.ok=TRUE);

	isNew <- rep(FALSE, argLen);
	if (any(noMatch <- (i == 0L))) { 
		.valCondition(sprintf(ngettext(sum(noMatch),
			"value %s of index %s is not a valid name of %s", 
			"values %s of index %s are not valid names of %s"),
			dqMsg(arg[noMatch]), sqMsg(.argName(.name)), 
			sqMsg("object")), ...);
		isNew[noMatch] <- TRUE;
		if (!exact) 
			arg[!noMatch] <- objNames[i[!noMatch]];		
	} else if (!exact)
		arg <- objNames[i];
	
	if (!several.ok && argLen > 1L) {
    	.valCondition(sprintf(ngettext(length(arg),
			"index %s selects %i element, but must select only 1",
			"index %s selects %i elements, but must select only 1"),
			sqMsg(.argName(.name)), length(arg)), ...);
		arg <- arg[1L];
		isNew <- isNew[1L];
	}
	
	if (!missing(retclass)) {
		retclass <- match.arg(retclass, c("logical", "integer", "character"));

		if (retclass == "integer") {
			arg <- i;
			if (any(noMatch))
				arg[noMatch] <- seq(from=objLen+1, to=objLen+sum(noMatch));
		} else if (retclass == "logical") {
			arg <- objNames %in% arg;
			if (any(noMatch)) {
				arg <- c(arg, rep(TRUE, sum(noMatch)));
				isNew <- c(rep(FALSE, objLen), rep(TRUE, sum(noMatch)));
			}
		}
	}

	if (mark.new) attr(arg, "is.new") <- isNew;	
	arg;
}