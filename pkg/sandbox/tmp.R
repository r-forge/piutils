################################################################################
# R PACKAGE:   piUtils
# FILE:        R/tmp.R
# DESCRIPTION: Scrapbook file for temporal code.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  17/04/2009
# LICENSE:     GPL-2
################################################################################
stop("not to be directly sourced!");

library(DevTools);
useDevLib();
devPath(file.path("C:", "ebe", "Work", "Projects", "PaRiS_R", "piUtils")); 

s("misc")
s("messages")
s("arg_validation")
s("valIndex")

# Run this hack after Roxygen to remove doc files of objects that are
# documented together with other objects:
toRm <- c("%LIKE%", "na.false", "colnamesToArgs", "customAttributesFun",
	"customAttrNames", 
	"customAttrNames.default", "customAttributesReplacement",
	"sqMsg", "dqMsg",
	"valIndex.default", "valIndex.logical", "valIndex.numeric",
		"valIndex.character");
 
unlink(file.path(devPath(), "man", paste(toRm, ".Rd", sep="")))

