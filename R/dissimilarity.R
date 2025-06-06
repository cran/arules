#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
# 			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
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

#' Classes dist, ar_cross_dissimilarity and ar_similarity --- Proximity
#' Matrices
#'
#' Simple classes to represent proximity matrices.
#'
#' For compatibility with
#' clustering functions in `R`, we represent dissimilarities as the
#' `S3` class `dist`.  For cross-dissimilarities and similarities, we
#' provide the `S4` classes `ar_cross_dissimilarities` and
#' `ar_similarities`.
#'
#'
#' @name proximity-classes
#' @family proximity classes and functions
#'
#' @section Objects from the Class: `dist` objects are the result of
#' calling the method `dissimilarity()` with one argument or any
#' `R` function returning a `S3 dist` object.
#'
#' `ar_cross_dissimilarity` objects are the result of calling the method
#' `dissimilarity()` with two arguments, by calls of the form
#' `new("similarity", ...)`, or by coercion from matrix.
#'
#' `ar_similarity` objects are the result of calling the method
#' [affinity()], by calls of the form `new("similarity", ...)`,
#' or by coercion from matrix.
#' @author Michael Hahsler
#' @seealso [stats::dist()], [proxy::dist()]
#' @keywords classes
setOldClass("dist")

#' @rdname proximity-classes
setClass("ar_similarity",
         contains = "matrix",
         representation(method = "character"))

#' @rdname proximity-classes
setClass("ar_cross_dissimilarity",
         contains = "matrix",
         representation(method = "character"))


#' Dissimilarity Matrix Computation for Associations and Transactions
#'
#' Provides the generic function `dissimilarity()` and the methods to
#' compute and returns distances for binary data in a `matrix`,
#' [transactions] or [associations] which
#' can be used for grouping and clustering. See Hahsler (2016) for an
#' introduction to distance-based clustering of association rules.
#'
#' @aliases dissimilarity dist
#' @family proximity classes and functions
#'
#' @param x the set of elements (e.g., `matrix`, [itemMatrix], [transactions],
#' [itemsets], [rules]).
#' @param y `NULL` or a second set to calculate cross dissimilarities.
#' @param method the distance measure to be used. Implemented measures are
#' (defaults to `"jaccard"`):
#'
#'    * `"affinity"`:
#'       measure based on the [affinity()], a similarity measure between
#'       items. It is defined as the average affinity between the items in two
#'       transactions (see Aggarwal et al. (2002)). If `x` is not the full
#'       transaction set `args` needs to contain either precalculated affinities
#'       as element `"affinities"` or the transaction set as element
#'       `"transactions"`.
#'    * `"cosine"`: the Cosine distance.
#'    *  `"dice"`: Dice's coefficient defined by Dice (1945).
#'      Similar to Jaccard but gives double the weight to agreeing items.
#'    * `"euclidean"`: the Euclidean distance.
#'    * `"jaccard"`: the number of items which occur in both elements
#'      divided by the total number of items in the elements (Sneath, 1957).  This
#'      measure is often also called: binary, asymmetric binary, etc.
#'    * `"matching"`: the matching coefficient defined by
#'       Sokal and Michener (1958). This coefficient gives the same weight to
#'       presents and absence of items.
#'    * `"pearson"` A distance calculated by \eqn{1 - r}
#'       if \eqn{r>1} and \eqn{1} otherwise, where \eqn{r} is the Pearson's correlation
#'       coefficient.
#'    * `"phi"`: same as `"pearson"`. Pearson's correlation coefficient
#'      reduces to the phi coefficient for the 2x2 contingency tables used
#'      here.
#'    * `"toivonen"`: Method described in Toivonen et al. (1995).  For
#'      rules this measure is only defined between rules with the same consequent.
#'      The distance between two rules is defined as the number of transactions
#'      which is covered by only one of the two rules.  The transactions used to
#'      mine the associations has to be passed on via `args` as element
#'      `"transactions"`.
#'   * `"gupta"`: Method described in Gupta et al. (1999).  The
#'      distance between two rules is defined as 1 minus the proportion of
#'      transactions which are covered by both rules in the transactions covered by
#'      each rule individually.  The transactions used to mine the associations has
#'      to be passed on via `args` as element `"transactions"`.
#'
#' @param args a list of additional arguments for the methods.
#' @param items logical; dissimilarity should be
#' calculated between transactions/associations (default) or items.
#' @param ... further arguments.
#' @return returns an object of class `dist`.
#' @author Michael Hahsler
#' @references Aggarwal, C.C., Cecilia Procopiuc, and Philip S. Yu. (2002)
#' Finding localized associations in market basket data.  _IEEE Trans. on
#' Knowledge and Data Engineering_ 14(1):51--62.
#'
#' Dice, L. R. (1945) Measures of the amount of ecologic association between
#' species. _Ecology_ 26, pages 297--302.
#'
#' Gupta, G., Strehl, A., and Ghosh, J. (1999) Distance based clustering of
#' association rules. _In Intelligent Engineering Systems Through
#' Artificial Neural Networks (Proceedings of ANNIE 1999)_, pages 759-764. ASME
#' Press.
#'
#' Hahsler, M. (2016) Grouping association rules using lift. In C.  Iyigun, R.
#' Moghaddess, and A. Oztekin, editors, _11th INFORMS Workshop on Data Mining
#' and Decision Analytics_ (DM-DA 2016).
#'
#' Sneath, P. H. A. (1957) Some thoughts on bacterial classification.
#' _Journal of General Microbiology_ 17, pages 184--200.
#'
#' Sokal, R. R. and Michener, C. D. (1958) A statistical method for evaluating
#' systematic relationships. _University of Kansas Science Bulletin_ 38,
#' pages 1409--1438.
#'
#' Toivonen, H., Klemettinen, M., Ronkainen, P., Hatonen, K. and Mannila H.
#' (1995) Pruning and grouping discovered association rules. _In
#' Proceedings of KDD'95_.
#' @keywords cluster models
#' @examples
#'
#' ## cluster items in Groceries with support > 5%
#' data("Groceries")
#'
#' s <- Groceries[, itemFrequency(Groceries) > 0.05]
#' d_jaccard <- dissimilarity(s, items = TRUE)
#' plot(hclust(d_jaccard, method = "ward.D2"), main = "Dendrogram for items")
#'
#' ## cluster transactions for a sample of Adult
#' data("Adult")
#' s <- sample(Adult, 500)
#'
#' ##  calculate Jaccard distances between sample transactions and do hclust
#' d_jaccard <- dissimilarity(s)
#' hc <- hclust(d_jaccard, method = "ward.D2")
#' plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Jaccard)")
#'
#' ## get 20 clusters and look at the difference of the item frequencies (bars)
#' ## for the top 20 items) in cluster 1 compared to the data (line)
#' assign <- cutree(hc, 20)
#' itemFrequencyPlot(s[assign == 1], population = s, topN = 20)
#'
#' ## calculate affinity-based distances between transactions and do hclust
#' d_affinity <- dissimilarity(s, method = "affinity")
#' hc <- hclust(d_affinity, method = "ward.D2")
#' plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Affinity)")
#'
#' ## cluster association rules
#' rules <- apriori(Adult, parameter = list(support = 0.3))
#' rules <- subset(rules, subset = lift > 2)
#'
#' ## use affinity to cluster rules
#' ## Note: we need to supply the transactions (or affinities) from the
#' ## dataset (sample).
#' d_affinity <- dissimilarity(rules,
#'   method = "affinity",
#'   args = list(transactions = s)
#' )
#' hc <- hclust(d_affinity, method = "ward.D2")
#' plot(hc, main = "Dendrogram for Rules (Affinity)")
#'
#' ## create 4 groups and inspect the rules in the first group.
#' assign <- cutree(hc, k = 3)
#' inspect(rules[assign == 1])
#'
setGeneric("dissimilarity", function(x,
                                     y = NULL,
                                     method = NULL,
                                     args = NULL,
                                     items = FALSE,
                                     ...) {
  standardGeneric("dissimilarity")
})

# this works with dense and sparse dgCMatrix matrices!
dissimilarity_internal <- function(x,
                                   y = NULL,
                                   method = NULL,
                                   args = NULL) {
  ## cross dissimilarities? Check y
  if (is.null(y)) {
    y <- x
    cross <- FALSE
  } else {
    cross <- TRUE
  }
  
  builtin_methods <- c("affinity",
                       "jaccard",
                       "matching",
                       "dice",
                       "cosine",
                       "euclidean",
                       "pearson",
                       "phi")
  
  if (is.null(method)) {
    ind <- 2
  } # Jaccard is standard
  else if (is.na(ind <- pmatch(tolower(method), tolower(builtin_methods)))) {
    stop(
      gettextf(
        "Value '%s' is not a valid abbreviation for a similarity method.",
        method
      ),
      domain = NA
    )
  }
  
  method <- builtin_methods[ind]
  
  ## affinity is special!
  if (ind == 1) {
    ## Affinity.
    ## for rules and itemsets we need transactions or affinities
    
    ## given affinities or transactions? Otherwise, calculate!
    if (!is.null(args$aff)) {
      affinities <- args$aff
    } else if (!is.null(args$trans)) {
      affinities <- affinity(args$trans)
    } else {
      affinities <- affinity(as(x, "matrix"))
    }
    
    ## Normalize transaction incidences by transaction length.
    x <- x / pmax(rowSums(x), 1)
    
    if (!cross) {
      dist <- 1 - stats::as.dist(x %*% affinities %*% t(x))
    } else {
      y <- y / pmax(rowSums(y), 1)
      dist <- new("ar_cross_dissimilarity", 1 - x %*% affinities %*% t(y))
    }
    
    ## Euclidean
  } else if (ind == 6) {
    c <- tcrossprod(x, y)
    a <- rowSums(x)
    b <- rowSums(y)
    
    dist <- (outer(a, b, "+") - 2 * c)^.5
    
  } else {
    if (!cross)
      y <- x
    
    ## prepare a, b, c, d (response table) for the rest of measures
    ## see: Gower, J. C. and P. Legendre.  1986.  Metric and
    ## Euclidean properties of dissimilarity coefficients.
    ## J. Classif. 3: 5 - 48.
    # a <- x %*% t(y)
    a <- tcrossprod(x, y)
    
    ## save some memory
    # b <- x %*% (1 - t(y))
    # c <- (1 - x) %*% t(y)
    # d <- ncol(x) - a - b - c
    
    # even faster code adapted from Leisch (2005): A toolbox for
    # K-centroids cluster analysis, Preprint.
    nx <- nrow(x)
    ny <- nrow(y)
    
    c <- matrix(rowSums(x), nrow = nx, ncol = ny) - a
    b <-
      matrix(rowSums(y),
             nrow = nx,
             ncol = ny,
             byrow = TRUE) - a
    
    # a_b_c <- matrix(rowSums(x), nrow = nx, ncol = ny) +
    # matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a
    
    if (ind == 2) {
      ## Jaccard == binary (Sneath, 1957)
      # dist <- dist(as(x, "matrix"), "binary")
      
      dist <- 1 - a / (a + b + c)
      # dist <- 1 - (a/a_b_c)
    } else if (ind == 3) {
      ## Matching Coefficient (Sokal and Michener, 1958)
      
      # we need d only here
      # d <- ncol(x) - a_b_c
      d <- ncol(x) - (a + b + c)
      
      dist <- 1 - (a + d) / (a + b + c + d)
      # dist <- 1 - ((a + d) / (a_b_c + d))
    } else if (ind == 4) {
      ## Dice Coefficient (Dice, 1945)
      dist <- 1 - 2 * a / (2 * a + b + c)
      # dist <- 1 - 2 * a / (a + a_b_c)
    } else if (ind == 5) {
      ## Cosine
      dist <- 1 - a / sqrt((a + b) * (a + c))
    } else if (ind == 7 || ind == 8) {
      ## phi and pearson correlation
      d <- ncol(x) - (a + b + c)
      phi <- ((b * c - a * d) / sqrt((a + b) * (c + d) * (a + c) * (b + d)))
      phi[phi < 0] <- 0
      dist <- 1 - phi
    }
    
  }
  # in case we divided by zero
  dist[is.nan(dist)] <- 0
  
  if (!cross) {
    # return a S3 "dist" object just add "ar_dissimilarity"
    dist <- stats::as.dist(dist)
    attr(dist, "method") <- method
    # class(dist) <- c("ar_dissimilarity", class(dist))
  } else {
    # return a S4 "ar_cross_dist" object
    dist <- new("ar_cross_dissimilarity", as(dist, "matrix"), method = method)
  }
  
  dist
}

#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "matrix"), function(x,
                                                             y = NULL,
                                                             method = NULL,
                                                             args = NULL, 
                                                             items = FALSE,
                                                             ...) {
  .nodots(...)
  ## Compute dissimilarities on binary data
  
  ## make sure the input is a 0-1 matrix or a logical matrix
  is.zeroone <- function(x) {
    (all(x == 0 | x == 1))
  }
  
  storage.mode(x) <- "numeric"
  if (!is.zeroone(x)) {
    stop("x is not a binary matrix (0-1 or logical)!")
  }
  
  ## cross dissimilarities? Check y
  if (!is.null(y)) {
    if (!is.matrix(y)) {
      stop("'y' not a matrix")
    }
    storage.mode(y) <- "numeric"
    if (!is.zeroone(y)) {
      stop("y is not a binary matrix (0-1 or logical)!")
    }
  }
  
  if (items) {
    x <- t(x)
    if (!is.null(y))
      y <- t(y)
  }
  
  dissimilarity_internal(x, y, method = method, args = args)
})


#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "itemMatrix"), function(x,
                                                                 y = NULL,
                                                                 method = NULL,
                                                                 args = NULL, 
                                                                 items = FALSE,
                                                                 ...) {
  .nodots(...)
  
  x <- as(as(x, "ngCMatrix"), "dMatrix")
  
  if (!is.null(y)) {
    y <- as(as(y, "ngCMatrix"), "dMatrix")
  }
  
  # note: ngCMatrix is stores transposed in itemMatrix!
  if (!items) {
    x <- t(x)
    if (!is.null(y))
      y <- t(y)
  }
  
  dissimilarity_internal(x, y, method = method, args = args)
})

#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "associations"), function(x,
                                                                   y = NULL,
                                                                   method = NULL,
                                                                   args = NULL,
                                                                   items = FALSE,
                                                                   ...) {
  .nodots(...)
  
  if (is.null(method)) {
    method <- "jaccard"
  } # Jaccard is standard
  
  builtin_methods <- c("gupta", "toivonen")
  
  if (!is.na(ind <- pmatch(tolower(method), tolower(builtin_methods)))) {
    method <- builtin_methods[ind]
    if (!is.null(y)) {
      stop("Cross dissimilarities not implemented for this method!")
    }
    trans <- args$trans
    if (is.null(trans)) {
      stop("Transactions needed in args for this method!")
    }
    
    if (ind == 1) {
      ## gupta: use tidlist intersection
      
      ## FIXME: tidlist operations should be made functions
      
      ## calculate tidlist for rules
      tids <- .associationTidLists(x, trans)
      
      m_X <- sapply(tids, length)
      n <- length(x)
      
      m <- matrix(nrow = n, ncol = n)
      for (i in 1:n) {
        for (j in i:n) {
          m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
          m[i, j] <- 1 - m_XiXj / (m_X[i] + m_X[j] - m_XiXj)
          m[j, i] <- m[i, j] ## dists are symmetric
        }
      }
      
      dist <- stats::as.dist(m)
      attr(dist, "method") <- method
      return(dist)
    } else {
      ## toivonen
      
      ## only one RHS allowed
      if (length(unique(rhs(x))) != 1) {
        stop("Only a single RHS allowed for this method!")
      }
      
      ## calculate tidlist for rules
      tids <- .associationTidLists(x, trans)
      
      m_X <- sapply(tids, length)
      n <- length(x)
      
      m <- matrix(nrow = n, ncol = n)
      for (i in 1:n) {
        for (j in i:n) {
          m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
          m[i, j] <- m_X[i] + m_X[j] - 2 * m_XiXj
          m[j, i] <- m[i, j] ## dists are symmetric
        }
      }
      
      dist <- stats::as.dist(m)
      attr(dist, "method") <- method
      return(dist)
    }
  }
  
  ## use methods for itemsets
  if (!is.null(y)) {
    y <- items(y)
  }
  dissimilarity(items(x),
                y,
                method = method,
                items = items,
                args = args)
})


## helper to compute tidLists for associations
## returns a list and not a tidList object
.associationTidLists <- function(x, trans) {
  I <- LIST(items(x), decode = FALSE)
  tlists <- LIST(as(trans, "tidLists"), decode = FALSE)
  
  tids <- list()
  for (i in seq_len(length(I))) {
    v <- I[[i]]
    tids[[i]] <- tlists[[v[1]]]
    if (length(v) > 1) {
      for (j in 2:length(v)) {
        tids[[i]] <- intersect(tids[[i]], tlists[[v[j]]])
      }
    }
  }
  
  tids
}
