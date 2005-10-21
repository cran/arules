###*******************************************************
### Function apriori
###
### Call the APRIORI algorithm


apriori <-  function(data, parameter = NULL, appearance = NULL, control = NULL)
  {
    ### prepare data
    data <- as(data, "transactions")
    items <- data@data
    if (is(appearance, "list")) appearance <- 
      as(c(appearance, list(labels = itemLabels(data))), "APappearance")
    appearance <- as(appearance, "APappearance")   
    control <- as(control, "APcontrol")
    parameter <- as(parameter, "APparameter")
   
    if(control@verbose) {
      ### print parameter
      cat("\nparameter specification:\n")
      print(parameter)
      cat("\nalgorithmic control:\n")
      print(control)
      cat("\n")
    }
    
    ### call apriori
    result <- .Call("rapriori", 
                 ## transactions
                 as(items@p, "integer"),
                 as(items@i, "integer"),
                 items@Dim,
		 ## parameter
		 parameter, control,
                 appearance,
                 PACKAGE = "arules")                  


    if (is(result, "rules"))  { 
      ### validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@lhs@data)
      validObject(result@rhs@data)
    
      result@lhs@itemInfo <- data@itemInfo
      result@rhs@itemInfo <- data@itemInfo
    } else {
      ### validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@items@data)
      
      result@items@itemInfo <- data@itemInfo
    }
    
    ### make quality a data.frame
    #result@quality <- as(result@quality, "data.frame")
    result@quality <- as.data.frame(result@quality)
     
    result
  }

