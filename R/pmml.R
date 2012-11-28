#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



##***************************************************************
## read/write PMML

write.PMML <- function(x, file) {
    if(!require("pmml")) stop("Package 'pmml' not installed!")

    saveXML(pmml(x), file=file)
}

read.PMML <- function(file) {
    doc <- xmlRoot(xmlTreeParse(file))

    ## check model type
    if(is.element("AssociationModel", names(doc)))
	return(.read.PMML.arules(doc))

    stop("File does not contain an AssociationModel.")
}

.read.PMML.arules <- function(doc) {

    ## just in case arules is not loaded  
#    if(!require("arules")) 
#	stop("Library arules is necessary to read Association Models!")

    ## extract model, items, itemsets (match item ids)

    model <- doc[["AssociationModel"]]

    items <- t(sapply(model[names(model) == "Item"], xmlAttrs))

    itemsets <- lapply(model[names(model) == "Itemset"], 
	    FUN = function(x) sapply(xmlChildren(x), xmlAttrs))
    itemsets <- lapply(itemsets, FUN=function(x) match(x, items[,"id"]))

    ## create arules itemsets object

    im <- encode(itemsets, itemLabels=items[,"value"])

    ## itemsets (= no rules)

    if(!is.element("AssociationRule", names(model))) {

	## get support and info
	## FIXME: maybe remove "id" and "numberofItems"

	quality <- t(sapply(model[names(model) == "Itemset"], 
			FUN=function(x) xmlAttrs(x)))
	mode(quality) <- "numeric"
	rownames(quality) <- NULL
	quality <- as.data.frame(quality)

	info <- xmlAttrs(doc[["AssociationModel"]]) 
	info <- list(ntransactions=as.integer(info["numberOfTransactions"]),
		support=as.numeric(info["minimumSupport"]),
		confidence=as.numeric(info["minimumConfidence"])
		)

	## create itemsets

	is <- new("itemsets", items=im, quality=quality, info=info)

	return(is)
    }else{

	## rules
	rules <- t(sapply(model[names(model) == "AssociationRule"], 
			xmlAttrs))

	quality <- rules[,!colnames(rules) %in% c("antecedent", "consequent")]
	mode(quality) <- "numeric"
	rownames(quality) <- NULL
	quality <- as.data.frame(quality)

	lhs <- rules[, "antecedent"]
	rhs <- rules[, "consequent"]

	## match with itemset ids!
	itemsetIDs <- sapply(model[names(model) == "Itemset"], 
		FUN=function(x) xmlAttrs(x)["id"])

	lhs <- im[match(lhs, itemsetIDs),]
	rhs <- im[match(rhs, itemsetIDs),]

	info <- xmlAttrs(doc[["AssociationModel"]]) 
	info <- list(ntransactions=as.integer(info["numberOfTransactions"]),
		support=as.numeric(info["minimumSupport"]),
		confidence=as.numeric(info["minimumConfidence"])
		) 

	## create rules
	rules <- new("rules", lhs = lhs, rhs = rhs, 
		quality = quality, info = info)

	return(rules)
    }
}


