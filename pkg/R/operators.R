################################################################################
# R PACKAGE:   piUtils
# FILE:        R/operators.R
# DESCRIPTION: New operators. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  30/06/2009
# LICENSE:     GPL-2
################################################################################

#' Operators returning a logical vector that determine whether each element in 
#' the first vector satisfy the regular expression provided as second argument.   
#' 
#' @title Match with Regular Expressions
#' @aliases `%LIKE%`
#' @usage x %like% y
#'  x %LIKE% y
#' @param x a vector that can be coerced to \code{character}.
#' @param y a string with a regular expression.
#' 
#' @return A logical vector of the same length as \code{x}.
#' @seealso A variety of useful operators in package \code{mvbutils}:  
#'  \code{\link[mvbutils]{mvbutils-operators}}.
#' @export `%like%` 

`%like%` <- function(x, y) 
	seq_along(x) %in% grep(y, x, ignore.case=TRUE);

#' @nord
#' @export `%LIKE%`
`%LIKE%` <- function(x, y) 
	seq_along(x) %in% grep(y, x);
