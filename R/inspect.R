setMethod("inspect", signature(x = "itemsets"),
    function(x, ...) {

        n_of_itemsets <- length(x)

        if(length(n_of_itemsets) == 0) return()
        ## Nothing to inspect here ...
        
        items <- as(items(x), "list")
        quality <- quality(x)

        ## Various lengths ...
        n_of_items <- sapply(items, length)
        entry_end_pos <- cumsum(n_of_items) + 1
        entry_beg_pos <- c(1, entry_end_pos[-n_of_itemsets]) + 1
        n_of_rows <- entry_end_pos[n_of_itemsets]

        ## Output.
        out <- matrix("", nr = n_of_rows, nc = 2 + NCOL(quality))

        ## Column 1: counts.
        tmp <- rep.int("", n_of_rows)
        tmp[entry_beg_pos] <- seq(length = n_of_itemsets)
        out[, 1] <- format(tmp)

        ## Column 2: items in the item sets, one per line.
        pre <- rep.int("  ", n_of_rows)
        pre[entry_beg_pos] <- "{"
        post <- rep.int(",", n_of_rows)
        post[entry_end_pos] <- "}"
        out[, 2] <-
            format(c("items",
                     paste(pre, unlist(items), post, sep = "")[-1]))
        
        ## Remaining columns: quality measures.
        for(i in seq(length = NCOL(quality))) {
            tmp <- rep.int("", n_of_rows)
            tmp[1] <- names(quality)[i]
            tmp[entry_end_pos] <- format(quality[[i]])
            out[, i + 2] <- format(tmp, justify = "right")
        }
        
        ## Output.
        cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))

    })

setMethod("inspect", signature(x = "rules"),
    function(x, ...) {

        n_of_rules <- length(x)
    
        if(n_of_rules == 0) return()
        ## Nothing to inspect here ...
    
        items_lhs <- as(lhs(x), "list")
        items_rhs <- as(rhs(x), "list")
        quality <- quality(x)

        ## Rewrite empty LHSs.
        ind <- sapply(items_lhs, length) == 0
        if(any(ind)) items_lhs[ind] <- ""

        ## Various lengths ...
        n_of_items_lhs <- sapply(items_lhs, length)
        n_of_items_rhs <- sapply(items_rhs, length)
        entry_end_pos <- cumsum(n_of_items_lhs + n_of_items_rhs - 1) + 1
        entry_beg_pos <- c(1, entry_end_pos[-n_of_rules]) + 1
        entry_mid_pos <- entry_beg_pos + n_of_items_lhs - 1
        lhs_pos <- unlist(mapply(seq, entry_beg_pos, entry_mid_pos,
                                 SIMPLIFY = FALSE))
        rhs_pos <- unlist(mapply(seq, entry_mid_pos, entry_end_pos,
                                 SIMPLIFY = FALSE))
        n_of_rows <- entry_end_pos[n_of_rules]

        out <- matrix("", nr = n_of_rows, nc = 4 + NCOL(quality))

        ## Column 1: counts.
        tmp <- rep.int("", n_of_rows)
        tmp[entry_beg_pos] <- seq(length = n_of_rules)
        out[, 1] <- format(tmp)

        ## Column 2: lhs.
        pre <- rep.int("  ", n_of_rows)
        pre[entry_beg_pos] <- "{"
        post <- rep.int("", n_of_rows)
        post[lhs_pos] <- ","
        post[entry_mid_pos] <- "}"
        tmp <- rep.int("", n_of_rows)
        tmp[lhs_pos] <- unlist(items_lhs)
        out[, 2] <-
            format(c("lhs", paste(pre, tmp, post, sep = "")[-1]))

        ## Column 3: '=>'
        tmp <- rep.int("", n_of_rows)
        tmp[entry_mid_pos] <- "=>"
        out[, 3] <- format(tmp)

        ## Column 4: rhs.
        pre <- rep.int("  ", n_of_rows)
        pre[entry_mid_pos] <- "{"
        post <- rep.int("", n_of_rows)
        post[rhs_pos] <- ","
        post[entry_end_pos] <- "}"
        tmp <- rep.int("", n_of_rows)
        tmp[rhs_pos] <- unlist(items_rhs)
        out[, 4] <-
            format(c("rhs", paste(pre, tmp, post, sep = "")[-1]))

        ## Remaining columns: quality measures.
        for(i in seq(length = NCOL(quality))) {
            tmp <- rep.int("", n_of_rows)
            tmp[1] <- names(quality)[i]
            tmp[entry_end_pos] <- format(quality[[i]])
            out[, i + 4] <- format(tmp, justify = "right")
        }
        ## Output.
        cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))

    })



setMethod("inspect", signature(x = "transactions"),
    function(x, ...) {

        n_of_itemsets <- length(x)

        if(length(n_of_itemsets) == 0) return()
        ## Nothing to inspect here ...
        
        items <- as(x, "list")
        transactionInfo <- transactionInfo(x)

        ## Various lengths ...
        n_of_items <- sapply(items, length)
        entry_end_pos <- cumsum(n_of_items) + 1
        entry_beg_pos <- c(1, entry_end_pos[-n_of_itemsets]) + 1
        n_of_rows <- entry_end_pos[n_of_itemsets]

        ## Output.
        out <- matrix("", nr = n_of_rows, nc = 2 + NCOL(transactionInfo))

        ## Column 1: counts.
        tmp <- rep.int("", n_of_rows)
        tmp[entry_beg_pos] <- seq(length = n_of_itemsets)
        out[, 1] <- format(tmp)

        ## Column 2: items in the item sets, one per line.
        pre <- rep.int("  ", n_of_rows)
        pre[entry_beg_pos] <- "{"
        post <- rep.int(",", n_of_rows)
        post[entry_end_pos] <- "}"
        out[, 2] <-
            format(c("items",
                     paste(pre, unlist(items), post, sep = "")[-1]))
        
        ## Remaining columns.
        for(i in seq(length = NCOL(transactionInfo))) {
            tmp <- rep.int("", n_of_rows)
            tmp[1] <- names(transactionInfo)[i]
            tmp[entry_end_pos] <- format(transactionInfo[[i]])
            out[, i + 2] <- format(tmp, justify = "right")
        }
        
        ## Output.
        cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))

    })
