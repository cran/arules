## IDs -> labels
setMethod("decode", signature(x = "numeric"),
    function(x, itemLabels) itemLabels[x])

setMethod("decode", signature(x = "list"),
    function(x, itemLabels) lapply(x, function(r) itemLabels[r])    
    )

## labels -> IDs
setMethod("encode", signature(x = "character"),
    function(x, itemLabels, itemMatrix = TRUE) {
        ## itemMatrix always is created from list
        if(itemMatrix == TRUE) 
        return(encode(list(x), itemLabels, itemMatrix == TRUE))

        ## regular encoding
        res <- which(itemLabels %in% x)
        if(length(res) < length(x))
        stop("Unknown item label in ", deparse(x))

        return(res)
    })

setMethod("encode", signature(x = "numeric"),
    function(x, itemLabels, itemMatrix = TRUE) {
        ## itemMatrix always is created from list
        if(itemMatrix == TRUE) 
        return(encode(list(x), itemLabels, itemMatrix == TRUE))

        ## regular encoding
        if(any(x > length(itemLabels)))
        stop("Too high label ID in ", deparse(x))

        return(x)
    })

setMethod("encode", signature(x = "list"),
    function(x, itemLabels, itemMatrix = TRUE) {
        ids <- lapply(x, function(i) encode(i, itemLabels, itemMatrix = FALSE))
        if(itemMatrix == FALSE) return(ids)

        ## create an itemMatrix
        ngC <- as(ids, "ngCMatrix")

        ## fix dim, if necessary (i.e., if items with high IDs
            ##   do not occur in ids)
        ngC@Dim <- c(length(itemLabels), ngC@Dim[2])    

        new("itemMatrix", data = ngC, 
            itemInfo = data.frame(labels = itemLabels))
    })

## recode to make compatible
setMethod("recode", signature(x = "itemMatrix"),
    function(x, itemLabels = NULL, match = NULL ,...) {

        ## match x with object in match
        if(!is.null(match)) itemLabels <- itemLabels(match)

        ## enlarge matrix
        x@data@Dim <- c(length(itemLabels), x@data@Dim[2])

        ## recode items
        items.index <- as.integer(match(itemLabels(x), itemLabels) - 1)
        if(any(is.na(items.index))) stop ("Items are incompatible!\nAll items in x have to be available in itemLabels or match.")

        x@data@i <- items.index[(x@data@i +1)]

        ## fix itemlabels
        if(!is.null(match)) itemInfo(x) <- itemInfo(match)
        else itemInfo(x) <- data.frame(labels = itemLabels)

        return(x)
    })	

