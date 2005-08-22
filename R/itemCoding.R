### IDs -> labels
setMethod("decode", signature(x = "numeric"),
    function(x, code, ...) {
        code[x]
    })

setMethod("decode", signature(x = "list"),
    function(x, code, ...) {
        lapply(x, function(r) code[r])     
    })


### labels -> IDs
setMethod("encode", signature(x = "character"),
    function(x, code, itemMatrix = TRUE) {
        ### itemMatrix always is created from list
        if(itemMatrix == TRUE) 
        return(encode(list(x), code, itemMatrix == TRUE))
        
        ### regular encoding
        res <- which(code %in% x)
        if(length(res) < length(x))
        stop("Unknown item label in ", deparse(x))

        return(res)
    })

setMethod("encode", signature(x = "numeric"),
    function(x, code, itemMatrix = TRUE) {
        ### itemMatrix always is created from list
        if(itemMatrix == TRUE) 
        return(encode(list(x), code, itemMatrix == TRUE))
        
        ### regular encoding
        if(any(x > length(code)))
        stop("Too high label ID in ", deparse(x))

        return(x)
    })

setMethod("encode", signature(x = "list"),
    function(x, code, itemMatrix = TRUE) {
        ids <- lapply(x, function(i) encode(i, code, itemMatrix = FALSE))
        if(itemMatrix == FALSE) return(ids)

        ### create an itemMatrix
        dgC <- as(ids, "dgCMatrix")
        
        ### fix dim, if necessary (i.e., if items with high IDs
        ###   do not occur in ids)
        dgC@Dim <- c(length(code), dgC@Dim[2])    
    
        new("itemMatrix", data = dgC, itemInfo = data.frame(labels = code))
    })

### recode to make compatible
setMethod("recode", signature(x = "itemMatrix"),
    function(x, code = NULL, match = NULL ,...) {
        
        ### match x with object in match
	if(!is.null(match)) code <- itemLabels(match)
	
	### enlarge matrix
	x@data@Dim <- c(length(code), x@data@Dim[2])

        ### recode items
        items.index <- as.integer(match(itemLabels(x), code) - 1)
	if(any(is.na(items.index))) stop ("Items are incompatible!\nAll items in x have to be available in code or match.")
	
	x@data@i <- items.index[(x@data@i +1)]
        
	### fix itemlabels
	if(!is.null(match)) itemInfo(x) <- itemInfo(match)
	else itemInfo(x) <- data.frame(labels = code)
   
        return(x)
    })	

