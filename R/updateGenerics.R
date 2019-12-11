# Function to update the generics list given new data
# 
# Author: Joe Perez-Rogers
# 
# @param parameters a list
# @param newdata the output from a pipeline task
# @param module.class one of \code{M1}, \code{M2}, \code{M3}, or \code{M4}
# 
# @return a \code{list} with the same names and dimensions as \code{parameters}
# 
# @examples
# \dontrun{
# generics <- list(x = matrix(runif(100,2,15),nrow=10),
#                  y = rep(c(0,1),5))
# newdata <- matrix(runif(50,2,15),nrow=5)
# module.class <- "M1"
#                    
# parameters <- updateGenerics(parameters,newdata,module.class)
# }
# @keywords internal

updateGenerics <- function(generics,newdata,module.class){

if(module.class=="M1"){
  
  generics$x <- newdata
  
} else if(module.class=="M2"){
  
  generics$x <- newdata[["x"]]
  generics$rank <- newdata[["rank"]]
  
} else if(module.class=="M3"){
  
  generics$x <- newdata
  
}

return(generics)

}