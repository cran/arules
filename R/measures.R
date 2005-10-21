###*******************************************************
### Functions for additional interest measures
###


# calculate all-confidence for existing (frequent) itemsets.
#
# Edward R. Omiecinski. Alternative interest measures for mining 
# associations in databases. IEEE Transactions on Knowledge and 
# Data Engineering, 15(1):57-69, Jan/Feb 2003.
#

setMethod("allConfidence",  signature(x = "itemsets"),
function(x,  transactions = NULL, itemSupport = NULL) {

  num_items <- dim(items(x))[2]

  if(is.null(itemSupport) && is.null(transactions)) {
    # get the support of all singleton itemsets from the itemsets
    # only workes from a full set of frequent itemsets!
    
    single_items <- x[size(items(x)) == 1]
    
    # get the col numbers we have support for
    single_support <- quality(single_items)$support
    single_itemIDs <- unlist(LIST(items(single_items), 
    	decode = FALSE))

    itemSupport <- numeric(length = length(single_itemIDs))
    itemSupport[single_itemIDs] <- single_support 
    
    # we have to check for NA later to see if we really got all
    # needed item supports
  
  }else if(is.null(itemSupport)) {
    # get item support from the transactions

    itemSupport <- itemFrequency(transactions, type = "relative")  
    if (length(itemSupport) != num_items) 
      stop("number of items in itemsets and transactions do not match.")
  
  }else{
    # we should have item support
    if (length(itemSupport) != num_items)
      stop("number of items in itemsets and itemSupport do not match.")
  }

  # create an itemset list
  itemset_list <- LIST(items(x), decode = FALSE)

  # calculate all-confidence using itemsets support and the 
  # singleton support of the most frequent item in the itemset
  # all-confidence(Z) = supp(Z) / max(supp(i elem Z))
 
  all_conf <-  quality(x)$support /
  sapply(itemset_list, function(i) max(itemSupport[i]))

  # check for insufficient item support (NAs) 
  if(any(is.na(all_conf))) warning("some values are missing because the itemset does not contain all needed item supports. provide itemSupport or transactions.")
  
  all_conf
})


# calculate hyperlift for existing rules.
#
# Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
# Implications of probabilistic data modeling for rule mining. 
# Report 14, Research Report Series, Department of Statistics and 
# Mathematics, Wirschaftsuniversität Wien, Augasse 2-6, 1090 Wien, 
# Austria, March 2005.
#

# hyperlift(X => Y) = c_X,Y / Q_d[C_X,Y] 
#
# where Q_d[C_X,Y] = qhyper(d, m = c_Y, n = length(trans.) - c_Y, k = c_X)
#
# c_X,Y = supp(X,Y) * length(transactions)
# c_X = supp(X,Y) / conf(X => Y) * length(transactions)
# c_Y = itemFrequency(transactions, type = "absolute") reordered for consequents
#
# this implements only hyperlift for rules with a single item in the consequent


setMethod("hyperLift",  signature(x = "rules"),
    function(x, transactions, d = 0.99) {
    c_XY <-  quality(x)$support * length(transactions)
    c_X <- c_XY / quality(x)$confidence
    cons <- unlist(LIST(rhs(x), decode = FALSE))

### check that the consequents are all singletons
    if (length(cons) != length(x)) stop("this implementation only works for
      rules with one item in the rhs.")

    c_Y <- itemFrequency(transactions, type = "absolute")[cons]

    Q <- qhyper(d, m = c_Y, n = length(transactions) - c_Y, k = c_X, 
      lower.tail = TRUE)
    hyperlift <- c_XY / Q

    hyperlift
    })


# calculate hyperconfidence for existing rules.
# (confidence level that we observe too high/low counts)
# 
# uses the model from:
# Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
# Implications of probabilistic data modeling for rule mining. 
# Report 14, Research Report Series, Department of Statistics and 
# Mathematics, Wirschaftsuniversität Wien, Augasse 2-6, 1090 Wien, 
# Austria, March 2005.
#
# since the counts are drawn from a hypergeometric distribution with
# known parameters (margials), we can calculate a confidence interval
# for each cell and report the associated confidence level 
# 1 - P[C_XY >= c_XY | c_X, c_Y] for complements, and
# 1 - P[C_XY < c_XY | c_X, c_Y] for substitutes.


setMethod("hyperConfidence",  signature(x = "rules"),
    function(x, transactions, complements = TRUE, significance = FALSE) {
    
    # significance: return significance levels instead of
    #   confidence levels
    
    t <- length(transactions)
    c_XY <-  quality(x)$support * t 
    c_X <- c_XY / quality(x)$confidence
    cons <- unlist(LIST(rhs(x), decode = FALSE))

### check that the consequents are all singletons
    if (length(cons) != length(x)) stop("this implementation only works for
      rules with one item in the rhs.")

    c_Y <- itemFrequency(transactions, type = "absolute")[cons]
    

    if(complements == TRUE)
    ### c_XY - 1 so we get P[C_XY >= c_XY] instead of P[C_XY > c_XY]
      res <- phyper(c_XY - 1, m=c_Y, n=t-c_Y, k=c_X, lower.tail = !significance)
    
    else
    ### substitutes; Pr[C_XY < c_XY]
      res <- phyper(c_XY, m=c_Y, n=t-c_X, k=c_X, lower.tail = significance)
 
    ### todo: check resulting NaN
 
    res
})
