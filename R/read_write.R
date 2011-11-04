#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler, Christian Buchta, 
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
## read/write functions

## for convenience [ceeboo 2007]
.rm.duplicates <- function(x) {
    n <- sapply(x, length, USE.NAMES = FALSE)
    x <- lapply(x, unique)
    n <- n - sapply(x, length, USE.NAMES = FALSE)
    if (any(n)) {
        n <- table(items = n)[-1]
        cat("distribution of transactions with duplicates:\n")
        print(n)
    }
    x
}

read.transactions <-
function(file, format = c("basket", "single"), sep = NULL, cols = NULL, rm.duplicates = FALSE, encoding="unknown")
{
    format <- match.arg(format)
    if (format == "basket") {
        if (is.null(sep))
            sep <- "[ \t]+"
        data <- strsplit(readLines(file, encoding=encoding), split = sep)
        if (!is.null(cols)) {
            if (!(is(cols, "numeric") && (length(cols) == 1)))
                stop("'cols' must be a numeric scalar for 'basket'.")
            cols <- as(cols, "integer")
            names(data) <- sapply(data, "[", cols)
            data <- lapply(data, "[", -cols)
        }
        if (rm.duplicates)
            data <- .rm.duplicates(data)
        return(as(data,"transactions"))   
    }
    
    ## If format is "single", have lines with TIDs and IIDs in the
    ## columns specified by 'cols'.

    ## If cols is a character vector of length 2 we assume the file
    ## has a header with colnames (added by F. Leisch)
    skip <- 0
    if(is(cols, "character") && (length(cols) == 2)){
        colnames <- scan(file = file, what="", sep = sep,
                         quiet = TRUE, nlines=1)
        cols <- match(cols, colnames)
        if(any(is.na(cols)))
            stop("'cols' does not match 2 entries in header of file.")
        skip <- 1
    }

    ## Else we get the numbers of the columns directly
    if (!(is(cols, "numeric") && (length(cols) == 2)))
        stop("'cols' must be a numeric or character vector of length 2 for 'single'.")
    
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(file = file, sep = sep, what = what, flush = TRUE,
                    quiet = TRUE, skip = skip)
    
    entries <- split(entries[[cols[2]]], entries[[cols[1]]])
    if (rm.duplicates)
        entries <- .rm.duplicates(entries)
    as(entries, "transactions")
}

## write transactions and associations
### FIXME: Quote does not work for basket format!

setMethod("WRITE", signature(x = "transactions"),
	function(x, file = "", format = c("basket", "single"), 
		sep=" ", quote=FALSE, ...) { 

	    format <- match.arg(format)
	    if (format == "basket") {
		l <- LIST(x)
		dat <- unlist(list(lapply(l, paste, collapse=sep)))
		if(quote) warning("Quote not implemented for basket format!")
		write(dat, file=file, ...)
	    } else { 
		l <- LIST(x)
		dat <- data.frame(transactionID=rep(names(l),lapply(l, length)), 
			item=unlist(l), row.names=NULL)
		write.table(dat, file = file, sep=sep, quote=quote, ...)
	    }
	    invisible(dat)
	}
	)


setMethod("WRITE", signature(x = "associations"),
    function(x, file = "", sep= " ", quote=FALSE, ...) 
    write.table(as(x, "data.frame"), file = file, sep=sep, quote=quote, ...)
)

