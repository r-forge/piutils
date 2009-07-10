################################################################################
# R PACKAGE:   piUtils 
# FILE:	       R/misc.R
# DESCRIPTION: Miscellaneous utilities that do not fit in any other
#              source file in the package.
# AUTHOR: 	   Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  15 April 2009
# LICENSE:     GPL-2
################################################################################
# NOTES: - See the set of renaming methods in package 'memisc'

#' These functions remove \code{NA}s from vectors or convert them to 
#' \code{FALSE}. 
#'
#' @title Handle NAs in Vectors
#' @aliases na.false
#' @usage na.rm(x)
#'  na.false(x)
#' @param x an \link[stats:is.recursive]{atomic} object. If \code{x} is not 
#'  atomic a warning is issued, and \code{x} is returned without changes.
#'
#' @return For \code{na.rm}, \code{x} with all \code{NA}s removed.
#' 
#'  For \code{na.false}, \code{x} (coerced to logical if necessary) with all 
#'  	\code{NA}s converted to \code{FALSE}.
#' @seealso \code{\link[stats:na.fail]{na.omit}} in package \pkg{stats} to 
#' 	rememeber the positions of the removed \code{NA}s.
#' @export

na.rm <- function(x) {
	if (!is.atomic(x))
		warning(gettextf("%s only works on atomic objects", sqMsg("na.rm")));
	x[!is.na(x)]
}

#' @nord
#' @export `%LIKE%`

na.false <- function(x) {
	if (!is.atomic(x))
		warning(gettextf("%s only works on atomic objects", sqMsg("na.rm")));	
	x <- as.logical(x);
	x[is.na(x)] <- FALSE;
	x;
}


# @title Find Duplicates
# @param x a \R object
# @export
# See http://www.nabble.com/Duplicates-and-duplicated-td23535003.html

#duplicated <- function(x) {	
#}

#' Removes leading and trailing whitespaces from character vectors.
#' 
#' @title Trim Whitespace
#' @param x a character vector, or object that can be coerced to one.
#' 
#' @return \code{x}, coerced if necessary to character, with leading and 
#'  trailing whitespaces removed from each element. Attributes are preserved.
#' @export

trim <- function(x) {
	if (length(x) == 0L) {
		x;
	} else {		
		result <- sub('^[[:space:]]+', '', x);
		result <- sub('[[:space:]]+$', '', result);
		attributes(result) <- attributes(x);
		result;
	}
}

#' This function transforms strings where each character represents a class
#' name into character vectors of class names. This is useful for easier 
#' typing of long enumerations of classes, such as those used in importing
#' functions like \code{\link{read.table}}.
#' 
#' The mapping from letters to class names is the following:
#' \describe{
#'	\item{i}{integer}
#'	\item{n}{numeric}
#'	\item{l}{logical}
#'	\item{c}{character}
#'	\item{x}{complex}
#'	\item{r}{raw}
#'	\item{f}{factor}
#'	\item{D}{Date}
#'	\item{P}{POSIXct}
#'	\item{t}{POSIXlt}
#'	\item{N}{NA_character_} 
#' }
#' Values outside this table produce warnings. 
#' 
#' @title Transform Initials into Class Names  
#' @param x a character vector.
#' 
#' @return A list with the same length as \code{x}, where each element
#'  is a character vector of class names whose length equals the number of 
#'  characters in the corresponding element of \code{x}. 
#' @seealso \code{\link{expandLogical}}.
#' @note This was inspired by \code{colClasses} in \code{R.utils}, 
#' 	replace by the code there which is better.
#' @export
#  TODO: Replace expandClasses() by R.utils::colClasses

expandClasses <- function(x) {
	unknowns <- character(0);
	result <- lapply(strsplit(as.character(x), NULL, fixed=TRUE), 
    	function(y) {			
			sapply(y, function(z)
				switch(z,
					i = "integer",
					n = "numeric",
					l = "logical",
					c = "character",
					x = "complex",
					r = "raw",
					f = "factor",
					D = "Date",
					P = "POSIXct",
					t = "POSIXlt",
					N = NA_character_, 
					{
						unknowns <<- c(unknowns, z);
						NA_character_;
    				}
				), USE.NAMES = FALSE					
			)	
		}
	)	
	
	if (length(unknowns)) {
		unknowns <- unique(unknowns);
		warning(sprintf(ngettext(length(unknowns),					
			"code %s not recognized",
			"codes %s not recognized"), 
			dqMsg(unknowns)));
	}
	result; 
}

#' This function transforms strings where each character represents a logical
#' value into logical vectors.
#' 
#' The mapping from letters to class names is the following:
#' \describe{
#'	\item{t or T}{TRUE}
#'	\item{f or F}{FALSE}
#'	\item{n or N}{NA}
#' } 
#' Values outside this table produce warnings.
#'  
#' @title Transform Initials into Logical Values  
#' @param x a character vector.
#' 
#' @return A list with the same length as \code{x}, where each element
#'  is a logical vector whose length equals the number of characters in the 
#'  corresponding element of \code{x}. 
#' @seealso \code{\link{expandClasses}}.
#' @export

expandLogical <- function(x) {
	unknowns <- character(0);
	result <- lapply(strsplit(as.character(x), NULL, fixed=TRUE), 
    	function(y) {			
			sapply(y, function(z)
				switch(z,
					t =, T = TRUE,
					f =, F = FALSE,
					n =, N = NA,
					{
						unknowns <<- c(unknowns, z);
						NA;
    				}
				), USE.NAMES = FALSE					
			)	
		}
	)	
	
	if (length(unknowns)) {
		unknowns <- unique(unknowns);
		warning(sprintf(ngettext(length(unknowns),					
			"code %s not recognized",
			"codes %s not recognized"), 
			dqMsg(unknowns)));
	}
	result; 
}

#' \code{matchPairs} returns a list of integer indexes of \code{x}
#' and integer indexes of \code{table} such that each position in the first
#' list element has as (first) match the corresponding position in the second 
#' list element.
#' 
#' This provides an easier-to-use interface to \code{\link{match}} for some
#' operations, as it allows to iterate only on matches (ignoring elements 
#' that do not match), and on both \code{x} and \code{table} at the same time.
#'   
#' @title Matching Pairs
#' @param x vector or \code{NULL}: the values to be matched. 
#' @param table vector or \code{NULL}: the values to be matched against.
#' @param exact logical flag; If \code{TRUE}, \link[match]{exact matching} is 
#'  performed, otherwise \link[pmatch]{partial matching} is used.
#' @param \dots arguments passed to the matching function. 
#' 
#' @return A list with two components:
#' 	\item{x}{integer vector of positions in \code{x} that have a match
#' 		on \code{table}}
#'  \item{table}{integer vector of the first positions in \code{table} that 
#' 		match against the corresponding element in \code{x}}
#' @seealso \code{\link{match}} and \code{\link{pmatch}}.
#' @export
#  TODO: Allow usage of MatchSpecs in matchPairs 

matchPairs <- function(x, table, exact = TRUE, ...) {
	result <- list();
	m <- if (exact)
		match(x, table, nomatch = 0L, ...)
	else 
		pmatch(x, table, nomatch = 0L, ...)
	isMatch <- (m > 0L);
	result$x <- which(isMatch);
	result$table <- m[isMatch];
	result;
}