################################################################################
# R PACKAGE:   piUtils
# FILE:        R/objects.R
# DESCRIPTION: Utilities for programming S3 classes. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  08/06/2009
# LICENSE:     GPL-2
################################################################################
# TODO: Write unit tests for customAttributes family of functions

#' These functions provide a way to retrieve and reset "custom" object 
#' attributes, which are those not included in the common set of attributes 
#' respected by subsetting functions such as \code{[}, by default 
#' "dim", "names", "dimnames", "row.names", "levels".   
#' 
#' Given than many base operations like extracting or replacing parts of
#' an object make it lose its "custom" attributes, removing and resetting 
#' them is a common operation when overloading these methods in S3 classes.
#' 
#' This set of functions provide a standardized interface for this operation.
#' They are all based on the generic method \code{customAttrNames}, which
#' has a default implementation but can be customized providing 
#' \link[base:useMethod]{methods} for any S3 class.
#'  
#' @title Custom Attributes
#' @name customAttributes
#' @aliases customAttrNames customAttrNames.default `customAttributes<-`
#' @usage customAttrNames(x, class = TRUE)
#'  customAttributes(x, class = TRUE)
#'  customAttributes(x, class = TRUE) <- value
#' @param x an \R object. 
#' @param class logical flag: include the "\code{class}" attribute?
#' @param value a list of values for the custom attributes, or \code{NULL}
#' 	to remove them.  
#' @param ... further parameters needed for future methods. Currently not used.
#' 
#' @return \code{customAttrNames} returns a character vector of attribute
#' 	names.
#' 
#' 	\code{customAttributes} returns a list with the values of the custom
#' 	attributes, where the set of attributes comes from invoking 
#'  \code{customAttrNames}.
#' 
#'  \code{customAttributes<-} updates the custom attributes of object \code{x}.
#'  This is frequently used to remove them with \code{value = NULL}.
#' 
#' @seealso \code{\link{attributes}}.
#' @export customAttrNames

customAttrNames <- function(x, class=TRUE, ...) UseMethod("customAttrNames");

#' @nord
#' @S3method customAttrNames default
# See \code{\link{customAttributes}}.

customAttrNames.default <- function(x, class=TRUE) {
	attrNames <- names(attributes(x));
	if (class)
	    attrNames[!attrNames %in% 
			c("dim", "names", "dimnames", "row.names", "levels")]
	else
	    attrNames[!attrNames %in% 
			c("class", "dim", "names", "dimnames", "row.names", "levels")]		
}

#' @name customAttributesFun
#' @nord
#' @export customAttributes
# See \code{\link{customAttributes}}.

customAttributes <- function(x, class=TRUE) {
	attributes(x)[names(attributes(x)) %in% customAttrNames(x, class=class)];
}

#' @name customAttributesReplacement
#' @nord
#' @export `customAttributes<-`
# See \code{\link{customAttributes}}.

`customAttributes<-` <- function(x, class=TRUE, value) {
	anm <- customAttrNames(x, class=class);
    if (!is.null(names(value)) && all(names(value) %in% anm) && 		
			any(names(value) != anm))
		value <- value[anm];
	attributes(x)[anm] <- value;
	x;
}

