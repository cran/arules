
##***************************************************************
## read/write for transactions

read.transactions <-
function(file, format = c("basket", "single"), sep = NULL, cols = NULL)
{
    format <- match.arg(format)
    if(format == "basket") {
        if(is.null(sep)) sep <- "[ \t]+"
        data <- strsplit(readLines(file), split = sep)
        if (!is.null(cols)) {
            if(!(is(cols, "numeric") && (length(cols) == 1)))
            stop("'cols' must be a numeric scalar for 'basket'.")
            cols <- as(cols, "integer")
            names(data) <- sapply(data, "[", cols)
            data <- lapply(data, "[", -cols)
        }
        return(as(data,"transactions"))   
    }
    ## If format is "single", have lines with TIDs and IIDs in the
    ## columns specified by 'cols'.
    if(!(is(cols, "numeric") && (length(cols) == 2)))
    stop("'cols' must be a numeric vector of length 2 for 'single'.")
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(file = file, sep = sep, what = what, flush = TRUE,
        quiet = TRUE)
    as(split(entries[[cols[2]]], entries[[cols[1]]]), "transactions")
}

## write

setMethod("WRITE", signature(x = "transactions"),
    function(x, ...) write.table(as(x, "data.frame"), ...)
)

