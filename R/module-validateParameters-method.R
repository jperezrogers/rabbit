#' Validate that input parameters conforms to required standards of the Task class
#' 
#' More detailed explaination here
#' 
#' @param parameters A data.frame with columns \code{parameter}, \code{class}, and \code{label}
#' 
#' @return TRUE if no errors are detected in \code{parameters}
#' 
#' @examples
#' parameters <- data.frame(
#'  parameter = c("x","y"),
#'  class = c("logical","numeric"),
#'  label = c("myX","myY"),
#'  stringsAsFactors=FALSE
#'  )
#' validateParameters(parameters)

# validate the input parameters slot for an object of class Task
validateParameters = function(parameters){
  
  # check that parameters is not null
  if(!is.null(parameters)){
    
    # check that parameters is a data.frame
    if(!is.data.frame(parameters)){
      stop("The parameters argument must be of class data.frame")
    }
    
    # check if parameters data.frame has colnames = c("parameter", "class", "label")
    if(!setequal(c("parameter","class","label"),colnames(parameters))){
      stop("The parameters argument must only contain named columns 'parameter', 'class', and 'label'")
    }
    
    # check that all values in the 'parameter' column are characters
    if(!all(sapply(parameters[["parameter"]],is.character))){
      stop("All 'parameter' values must be of class character. Try setting stringsAsFactors=F when defining your data.frame.")
    }
    
    # make sure that every parameter is unique, if not, stop()
    if(!nrow(parameters)==length(unique(parameters[["parameter"]]))){
      stop("All 'parameter' values must be unique")
    }
    
    # check that all values in the 'label' column are characters
    if(!all(sapply(parameters[["label"]],is.character))){
      stop("All 'label' values must be of class character")
    }
    
    # make sure that every parameter has a unique label, if not, stop()
    if(!nrow(parameters)==length(unique(parameters[["label"]]))){
      stop("All parameters must have a unique label")
    }
    
    # check that all values in the 'class' column are characters
    if(!all(sapply(parameters[["class"]],is.character))){
      stop("All 'class' values must be of class character")
    }
    
    # check that all values provided in the class column exist
    for(i in 1:nrow(parameters)){
      if(!exists(parameters$class[i])){
        stop(paste(parameters$class[i],"is not a valid class"))
      }
    }
  }
}