################################################################################
# R PACKAGE:   piUtils
# FILE:        R/names.R
# DESCRIPTION: Utilities related to object names. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  30/06/2009
# LICENSE:     GPL-2
################################################################################

#' \code{hasNames} verifies whether each element of an object has a valid
#' non-empty name. 
#' 
#' Empty names are all when \code{names(x)} is \code{NULL}, or otherwise
#' all names that are either \code{NA} or empty strings (\code{""}).  
#' 
#' @title Has Each Element a Name?
#' @param x a \R object.
#' @param dim integer. If specified, the corresponding dimension names 
#'  (using \code{\link{dimnames}}) are used instead of the plain names
#'  (using \code{\link{names}}).
#' 
#' @return A logical vector of the same length as \code{x} telling whether
#' 	each element of \code{x} has a non-empty and non-\code{NA} name.
#' @seealso \code{\link{names}}, \code{\link{dimnames}}.
#' @export

hasNames <- function(x, dim) {
	xNames <- if (missing(dim) || length(dim) == 0L) names(x) 
		else dimnames(x)[[as.integer(dim)[1L]]];
	if (is.null(xNames)) {
		xLen <- if (missing(dim) || length(dim) == 0L) length(x) 
			else dim(x)[as.integer(dim)[1L]];
		rep(FALSE, length.out=xLen); 
	} else (!is.na(xNames) & xNames != "")
}

#' Assigns names to unnamed elements of an object using a
#' vector of "default" names, with a strategy similar to function
#' arguments matching.
#' 
#' Every unnamed element of \code{x} (as returned by \code{\link{hasNames}}),   
#' is assigned sequentially a name from \code{names} that is not already
#' a name in \code{x} (using exact matching). This is similar to how
#' function arguments matching is done (although partial matching is used
#' there.) 
#' 
#' @title Assign Empty Names from a Vector of Names   
#' @param x a \R object.
#' @param names character vector of names to sequentially assign to empty
#'  names of \code{x}. 
#' 
#' @return A character vector of the same length as \code{x} to be used
#'  as names for \code{x}.
#' @seealso \code{\link{hasNames}}.
#' @export

emptyNamesFrom <- function(x, names) {
	xNames <- names(x);
	names <- unique(as.character(names));
	names <- names[!is.na(names) & names != ""]; 
		
	if (!any(unnamed <- !hasNames(x)) || length(names) == 0L)
		return(xNames);
	if (is.null(xNames))
		xNames <- rep("", length.out=length(x));
	
	if (any(unused <- !names %in% xNames)) {
		nn <- min(sum(unnamed), sum(unused));
		xNames[unnamed][1:nn] <- names[unused][1:nn]
	}
	xNames;
}

#' Transforms character vectors of names from/to "argument notation" 
#' (all lowercase, with words separated by dots) to/from character vectors of 
#' names in "column notation" (camel case, with the first letter of each word 
#' capitalized). 
#' e.g. c("pfl.id", "col.names") from/to c("PflId", "ColNames"). 
#' \code{NA}s are maintained.
#' 
#' @title Change Names between Argument and Colname Notation
#' @aliases colnamesToArgs
#' @usage argsToColnames(...)
#'  colnamesToArgs(...)
#' @param ... strings to change. 
#' 
#' @return A character vector.
#' @export

argsToColnames <- function(...) {
	result <- as.character(unlist(list(...)));
	isNA <- is.na(result);

	result <- sapply(
		lapply(
			strsplit(result, ".", fixed=TRUE),
				function(x) { substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x }
			),
	paste, collapse="");

	result[isNA] <- as.character(NA);
	result;
}

#' @nord
#' @export

colnamesToArgs <- function(...) {
	result <- as.character(unlist(list(...)));
	isNA <- is.na(result);

	result <- tolower(gsub("(.+)([[:upper:]])", "\\1\\.\\2", result));

	result[isNA] <- as.character(NA);
	result;
}