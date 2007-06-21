
##***************************************************************
## read/write for transactions

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
function(file, format = c("basket", "single"), sep = NULL, cols = NULL, rm.duplicates = FALSE)
{
    format <- match.arg(format)
    if (format == "basket") {
        if (is.null(sep))
            sep <- "[ \t]+"
        data <- strsplit(readLines(file), split = sep)
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
    if (!(is(cols, "numeric") && (length(cols) == 2)))
        stop("'cols' must be a numeric vector of length 2 for 'single'.")
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(file = file, sep = sep, what = what, flush = TRUE,
        quiet = TRUE)
    entries <- split(entries[[cols[2]]], entries[[cols[1]]])
    if (rm.duplicates)
        entries <- .rm.duplicates(entries)
    as(entries, "transactions")
}

## write

setMethod("WRITE", signature(x = "transactions"),
    function(x, ...) write.table(as(x, "data.frame"), ...)
)

