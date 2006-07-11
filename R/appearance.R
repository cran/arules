###*******************************************************
### Class APappearance
###
### parameters for item appearance in APRIORI


setAs("NULL", "APappearance",
function(from, to) { new(to) })

setAs("list", "APappearance", 
function(from, to) {
  if (!length(from)) return(new("APappearance"))

  if (is.null(from$labels)) 
    stop("labels are missing")

  args = c("lhs", "rhs", "both", "none", "items")
  other = c("default", "labels")
  if(!all(names(from) %in% c(args, other))) 
    stop(paste(names(from)[!names(from) %in% c(args, other)], 
    "is an unknown appearance indicator, use:", 
    paste(args, collapse=" "), collapse=", "))
  if (is.null(from$default)) from$default = "both"
  
  set <- c()
  items <- c()
  for (i in 1:length(args)) {
    indicator <- from[[args[i]]]
    if(is.null(indicator)) add_items <- NULL
    else if(!all(indicator %in% from$labels))
      stop(paste(indicator[!indicator %in% from$labels], 
        "is an unknown item label", collapse=", "))
    else add_items <- unlist(sapply(indicator,
      function(x) { which(from$labels == x) - 1 }))
    items <- c(items, add_items)
    set <- c(set, length(add_items))
  }
    
  # check NA's
  return(new("APappearance", default = from$default, items = as.integer(items),
        set = set, labels = from$labels))
}) 



