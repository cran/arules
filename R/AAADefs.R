###*******************************************************
### common definitions for arules

.types <- function(method = "apriori") {
  targets <- c("frequent itemsets", "maximally frequent itemsets", "closed frequent itemsets", "rules", "hyperedgesets")
    methods <- c("apriori", "eclat")
    method <- match.arg(tolower(method), methods)
    if (method == "eclat") return(targets[1:3])
    else return(targets)
}

.aremtypes <- function() {
  c("none",      ## no additional evaluation measure
      "diff",      ## absolute confidence difference
      "quot",      ## difference of conf. quotient to 1
      "aimp",      ## abs. diff. of improvement to 1
      "info",      ## information difference to prior
      "chi2")      ## normalized chi^2 measure
}


.list2object <-  function(from, to) {
  if (!length(from)) return(new(to)) 
  n = names(from)
  s = slotNames(to)
  p = pmatch(n, s)
  if(any(is.na(p)))
	stop(paste("\nInvalid slot name(s) for class",
	      to, ":", paste(n[is.na(p)], collapse=" ")))
  names(from) = s[p]
  do.call("new", c(from, Class=to))
  }


