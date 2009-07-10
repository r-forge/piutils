################################################################################
# R PACKAGE:   piUtils
# FILE:        R/messages.R
# DESCRIPTION: Utilities for program messages, like warnings, errors, or other 
#			   information shown in the console.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  3 April 2009
# LICENSE:     GPL-2
################################################################################

#' Prepare vectors for being included in messages such as warnings or errors, 
#' coercing them to character and concatenating all its elements into a 
#' single string of unique, non-localized, quoted strings.
#' 
#' Because these functions are frequently used to show multiple values in a 
#' warning on error message on the console, which is shown in a single line,  
#' you may frequently want to restrict to a maximum number of unique values, 
#' setting parameters \code{max} and \code{unique}. 
#' 
#' @title Quote Text in Messages 
#' @aliases nqMsg sqMsg dqMsg
#' @usage nqMsg(x, collapse = ",", unique = TRUE, max = NULL, max.char = NULL)
#'  sqMsg(x, collapse = ",", unique = TRUE, max = NULL, max.char = NULL)
#'  dqMsg(x, collapse = ",", unique = TRUE, max = NULL, max.char = NULL)
#' 
#' @param x an \R object, to be coerced to a character vector. 
#' @param collapse an optional character string to separate the results, 
#' 	or \code{NULL} if vector elements should not be concatenated. 
#' 	This is passed to \code{\link{paste}}.
#' @param max integer with the maximum number of elements to return. When
#'	this maximum is exceeded, the last element becomes "..."
#' @param max.char integer with the maximum number of characters in the 
#'  return. When
#'	this maximum is exceeded, the last element becomes "..." 	 	
#'
#' @return A character vector (of length 1 if \code{collapse} is not 
#' 	\code{NULL}) of non-quoted (\code{nqMsg}), single (\code{sqMsg}), or 
#'  double-quoted strings (\code{dqMsg}).
#' @seealso \code{\link{shorten}}. \code{\link[base]{sQuote}} and 
#'  \code{\link[base:dQuote]{dQuote}} for using localized quoting characters. 
#' @export 

nqMsg <- function(x, collapse=",", unique=TRUE, max=NULL, max.char=NULL) {
	if (unique) x <- unique(x)	
	if (length(max) && length(x) > as.integer(max[1])) {
    	x <- x[seq(from=1, to=as.integer(max[1])+1)];
		x[length(x)] <- "...";
	}
	x <- paste(x, sep="", collapse=collapse);
	if (length(max.char) && nchar(x) > max.char)
		paste(substring(x, 1, max.char), "...", sep="")
	else x;
}

#' @nord
#' @export

sqMsg <- function(x, ...) 
	nqMsg(paste("'", as.character(x), "'", sep=""), ...);

#' @nord
#' @export

dqMsg <- function(x, ...) 
	nqMsg(paste("\"", as.character(x), "\"", sep=""), ...);

#' @title Shorten Strings for Messages
#' @param x a character vector.
#' @param max.char integer. Maximum number of characters for each element of
#'  \code{x}. If this is \code{NA} or has zero length, \code{x} is returned 
#'  (coerced to character.)
#' 
#' @return \code{x}, where characters beyond \code{max.char} have been
#'  replaced by \code{...} 
#' @seealso \code{\link{nqMsg}}.
#' @export

shorten <- function(x, max.char=50) {
	x <- as.character(x);
	if (invalid(max.char)) 
		x
	else
		ifelse(nchar(x) > max.char, 
			paste(substring(x, 1, max.char), "...", sep=""), x);
}
