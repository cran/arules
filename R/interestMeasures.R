##*******************************************************
## Functions for additional interest measures
##

## measures for itemsets

setMethod("interestMeasure",  signature(x = "itemsets"),
    function(x, method, transactions = NULL, ...) {

        builtin_methods <- c("allConfidence", "crossSupportRatio")

        if(is.na(ind <- pmatch(tolower(method),
                    tolower(builtin_methods))))
        stop(gettextf("Value '%s' is an invalid method.", method), domain = NA)

        method <- builtin_methods[ind]

        return(.basicItemsetMeasures(x, method, transactions))
    })



.basicItemsetMeasures <- function(x, method, transactions = NULL) {

    if(is.null(quality(x)$support)) stop("The following measure in the quality slot of x is missing: support.")
    num_items <- dim(items(x))[2]

    if(is.null(transactions)) {
        ## get the support of all singleton itemsets from the itemsets
        ## only workes from a full set of frequent itemsets!

        single_items <- x[size(items(x)) == 1]

        ## get the col numbers we have support for
        single_support <- quality(single_items)$support
        single_itemIDs <- unlist(LIST(items(single_items), 
                decode = FALSE))

        itemSupport <- numeric(length = length(single_itemIDs))
        itemSupport[single_itemIDs] <- single_support 

        ## we have to check for NA later to see if we really got all
        ## needed item supports

    }else{
        ## get item support from the transactions

        itemSupport <- itemFrequency(transactions, type = "relative")  
        if (length(itemSupport) != num_items) 
        stop("number of items in itemsets and transactions do not match.")

    }

    ## create an itemset list
    itemset_list <- LIST(items(x), decode = FALSE)

    ## calculate all-confidence for existing (frequent) itemsets.
    ##
    ## Edward R. Omiecinski. Alternative interest measures for mining 
    ## associations in databases. IEEE Transactions on Knowledge and 
    ## Data Engineering, 15(1):57-69, Jan/Feb 2003.
    ##
    ## calculate all-confidence using itemsets support and the 
    ## singleton support of the most frequent item in the itemset
    ## all-confidence(Z) = supp(Z) / max(supp(i elem Z))

    if(method == "allConfidence") 
    measure <-  quality(x)$support /
    sapply(itemset_list, function(i) max(itemSupport[i]))


    ## calculate the cross-support ratio 
    ## used to eliminate cross support patterns which contain item with 
    ## extremely differnt support. These patterns tend to be spurious 
    ## (i.e., one item which occurs in virtually all transactions and some very 
        ##  rare items)
    ##
    ## Hui Xiong, Pang-Ning Tan, Vipin Kumar. Mining Strong Affinity Association 
    ## Patterns in Data Sets with Skewed Support. Third IEEE International
    ## Conference on Data Mining, Melbourne, Florida, November 19 - 22, 2003.
    ##

    if(method == "crossSupportRatio")
    measure <-  
    sapply(itemset_list, function(i) min(itemSupport[i])) /
    sapply(itemset_list, function(i) max(itemSupport[i]))


    ## check for insufficient item support (NAs) 
    if(any(is.na(measure))) warning("some values are missing because 'x' does not contain all needed item supports. provide transactions.")

    return(measure)
}

## measures for rules

setMethod("interestMeasure",  signature(x = "rules"),
    function(x, method, transactions = NULL, ...) {

        builtin_methods <- c("leverage", "hyperLift", "hyperConfidence", 
            "improvement", "chiSquare", 
            "cosine", "conviction", "gini", "oddsRatio", "phi") 

        if(is.na(ind <- pmatch(tolower(method),
                    tolower(builtin_methods))))
        stop(gettextf("Value '%s' is an invalid method.", method), domain = NA)

        method <- builtin_methods[ind]

        if(is.null(quality(x)$support)) stop("The following measure in the quality slot of x is missing: support.")
        if(is.null(quality(x)$confidence)) stop("The following measure in the quality slot of x is missing: confidence.")

        if(method == "improvement") return(.improvement(x))

        ## all other methods need transactions!
        if (is.null(transactions)) stop("For this method you need to specify the transactions used to mine 'x'.")

        if(method == "hyperLift") return(.hyperLift(x, transactions, ...))
        if(method == "hyperConfidence") 
        return(.hyperConfidence(x, transactions, ...))

        ## all other methods are implemented here
        return(.basicRuleMeasure(x, method, transactions))

        stop("Specified method not implemented.")
    })


## calculate hyperlift for existing rules.
##
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
## Implications of probabilistic data modeling for rule mining. 
## Report 14, Research Report Series, Department of Statistics and 
## Mathematics, Wirschaftsuniversität Wien, Augasse 2-6, 1090 Wien, 
## Austria, March 2005.

## hyperlift(X => Y) = c_X,Y / Q_d[C_X,Y] 
##
## where Q_d[C_X,Y] = qhyper(d, m = c_Y, n = length(trans.) - c_Y, k = c_X)
##
## c_X,Y = supp(X,Y) * length(transactions)
## c_X = supp(X,Y) / conf(X => Y) * length(transactions)
## c_Y = itemFrequency(transactions, type = "absolute") reordered for consequents
##
## this implements only hyperlift for rules with a single item in the consequent


.hyperLift <- function(x, transactions, d = 0.99) {

    c_XY <-  quality(x)$support * length(transactions)
    c_X <- c_XY / quality(x)$confidence

    ## cons <- unlist(LIST(rhs(x), decode = FALSE))
    ## that's low-level but way faster!
    cons <- x@rhs@data@i+1

    ## check that the consequents are all singletons
    if (length(cons) != length(x)) stop("this implementation only works for
        rules with one item in the rhs.")

    c_Y <- itemFrequency(transactions, type = "absolute")[cons]
    names(c_Y) <- NULL

    Q <- qhyper(d, m = c_Y, n = length(transactions) - c_Y, k = c_X, 
        lower.tail = TRUE)
    hyperlift <- c_XY / Q

    hyperlift
}


## calculate hyperconfidence for existing rules.
## (confidence level that we observe too high/low counts)
## 
## uses the model from:
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
## Implications of probabilistic data modeling for rule mining. 
## Report 14, Research Report Series, Department of Statistics and 
## Mathematics, Wirschaftsuniversität Wien, Augasse 2-6, 1090 Wien, 
## Austria, March 2005.


.hyperConfidence <- function(x, transactions, complements = TRUE, 
    significance = FALSE) {

    ## significance: return significance levels instead of
    ##   confidence levels

    t <- length(transactions)
    c_XY <-  quality(x)$support * t 
    c_X <- c_XY / quality(x)$confidence


    ## calculate c_Y from lift or count it from transactions
    if(!is.null(quality(x)$lift)) 
    c_Y <- quality(x)$confidence / quality(x)$lift * t
    else {
        ##cons <- unlist(LIST(rhs(x), decode = FALSE))
        ## that's low-level but way faster!
        cons <- x@rhs@data@i+1

        ## check that the consequents are all singletons
        if (length(cons) != length(x)) stop("this implementation only works for
            rules with one item in the rhs.")
        c_Y <- itemFrequency(transactions, type = "absolute")[cons]
    }

    if(complements == TRUE)
    ## c_XY - 1 so we get P[C_XY < c_XY] instead of P[C_XY <= c_XY]
    res <- phyper(c_XY - 1, m=c_Y, n=t-c_Y, k=c_X, lower.tail = !significance)

    else
    ## substitutes; Pr[C_XY > c_XY]
    res <- phyper(c_XY, m=c_Y, n=t-c_X, k=c_X, lower.tail = significance)

    ## todo: check resulting NaN
    res
}


## Minimum Improvement (Bayardo et al. 1999)
## Let the improvement of a rule be defined as the minimum
## difference between its confidence and the confidence of any
## proper sub-rule with the same consequent.

.improvement <- function(x) {

    conf <- quality(x)$confidence
    imp <- numeric(length(x))

    ## find sets of rules w/the same rhs 
    rhsList <- LIST(rhs(x), decode = FALSE)
    rhsUnique <- unique(rhsList)

    for(i in rhsUnique) {
        sameRhs <- which(is.element(rhsList, i))
        lhsSameRhs <- lhs(x)[sameRhs]

        for(j in sameRhs) {
            ## find subsets
            subRules <- sameRhs[is.superset(lhs(x)[j], 
                    lhsSameRhs, proper = TRUE)]

            ## calculate improvement
            imp[j] <- conf[j] - suppressWarnings(max(conf[subRules]))
        }
    }

    return(imp)
}


## more measures
## see Tan et al. Introduction to Data Mining, 2006

.basicRuleMeasure <- function(x, method, transactions) {

    N <- length(transactions)
    f11 <-  quality(x)$support * N
    f1x <- f11 / quality(x)$confidence

    ## calculate fx1 from lift or count it from transactions
    if(!is.null(quality(x)$lift)) 
    fx1 <- quality(x)$confidence / quality(x)$lift * N
    else {
        ##cons <- unlist(LIST(rhs(x), decode = FALSE))
        ## that's low-level but way faster!
        cons <- x@rhs@data@i+1

        ## check that the consequents are all singletons
        if (length(cons) != length(x)) stop("this implementation only works for
            rules with one item in the rhs.")
        fx1 <- itemFrequency(transactions, type = "absolute")[cons]
    }

    f0x <- N - f1x
    fx0 <- N - fx1

    f10 <- f1x - f11
    f01 <- fx1 - f11
    f00 <- f0x - f01

    if(method == "cosine") return(f11 / sqrt(f1x*fx1))
    if(method == "conviction") return(f1x*fx0 /(N*f10))
    if(method == "gini") return(
        f1x/N * ((f11/f1x)^2 + (f10/f1x)^2) - (fx1/N)^2 +
        f0x/N * ((f01/f0x)^2 + (f00/f0x)^2) - (fx0/N)^2
    )
    if(method == "oddsRatio") return(f11*f00/(f10*f01))
    if(method == "phi") return((N*f11-f1x*fx1) / sqrt(f1x*fx1*f0x*fx0))
    if(method == "leverage") return(f11/N - (f1x*fx1/N^2))


    ## this one is from Bing Liu, Wynne Hsu, and Yiming Ma (1999) 
    if(method == "chiSquare") {

        chi2 <- c()
        for(i in 1:length(x)) {
            fo <- matrix(c(f00[i], f01[i], f10[i], f11[i]), ncol=2)
            fe <- tcrossprod(c(fx0[i], fx1[i]), c(f0x[i], f1x[i])) /N
            ## check if approximation is ok
            ## we don't do this now
            ##if(any(fe < 5)) chi2[i] <- NA
            ##else 
            chi2[i] <- sum((fo - fe)^2 / fe)
        }

        ## the chi square test has 1 df for a 2x2 contingency table. 
        ## The critical value at alpha=0.05 is:
        ## qchisq(0.05, df =1, lower.tail=FALSE)
        ## [1] 3.841459

        return(chi2)
    }

    stop("Specified method not implemented.")
}

