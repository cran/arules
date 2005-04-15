eclat <-  function(data, parameter = NULL, control = NULL)
  {
    
    ### prepare data
    data <- as(data, "transactions")
    items <- data@data
    parameter <- as(parameter, "ECparameter")
    control <- as(control, "ECcontrol")
    
    ### print parameter
    cat("\nParameter specification:\n")
    print(parameter)
    if(control@verbose) {
      cat("\nAlgorithmic control:\n")
      print(control)
    }
    cat("\n")

    ### call eclat
    result <- .Call("reclat", 
                 ## Transactions
                 as(items@p, "integer"),
                 as(items@i, "integer"),
                 ## parameter
                 parameter, control,
                 PACKAGE = "arules")                  
    
    ### copy itemInfo
    result@items@itemInfo <- data@itemInfo
    
    ### make sure quality is a data.frame
    #result@quality <- as(result@quality, "data.frame")
    result@quality <- as.data.frame(result@quality,)
    
    ### make sure tid list itemInfo is ok
    if (!is.null(result@tidList)) {
      result@tidList@itemInfo <- data.frame(labels = labels(result))
      result@tidList@transactionInfo <- data@transactionInfo
    }
    
    cat("\nResult: ")
    print(result)
	  

    result
  }

