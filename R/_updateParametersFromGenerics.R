#' Function to update Task parameter list based on a list of generic variables
#' 
#' Author: Joe Perez-Rogers
#' 
#' @param parameters a list
#' @param generics a list of generic variables
#' @param module.class one of \code{M1}, \code{M2}, \code{M3}, or \code{M4}
#' 
#' @return a \code{list} with the same names and dimensions as \code{parameters}
#' 
#' @examples
#' generics <- list(x = matrix(runif(100,2,15),nrow=10),
#'                    y = rep(c(0,1),5))
#' parameters <- list()
#' module.class <- "M2"
#'                    
#' parameters <- .updateParametersFromGenerics(parameters,generics,module.class)

.updateParametersFromGenerics <- function(parameters,generics,module.class){
  
  if(module.class=="M1"){
    
    parameters[["x"]] <- generics$x
    
  } else if(module.class=="M2"){
    
    parameters[["x"]] <- generics$x
    parameters[["y"]] <- generics$y
    
    if(!is.null(data)){
      parameters[["data"]] <- generics$data
    }
    
  } else if(module.class=="M3"){
    
    parameters[["x"]] <- generics$x
    parameters[["rank"]] <- generics$rank
    
  } else if(module.class=="M4"){
    
    parameters[["x"]] <- generics$x
    parameters[["y"]] <- generics$y
    if("data"%in%names(parameters)){
      parameters[["data"]] <- generics$data
    }
    
    parameters[["testdata"]] <- generics$testdata[row.names(generics$x),]
    
  }
  
  return(parameters)
}