#' Create a new Task within a Module in an existing Pipeline
#' 
#' Author: Joe Perez-Rogers
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param type the type of module to add. One of 'M1', 'M2', or 'M3'
#' @param label a character string
#' 
#' @return function will execute silently
#' 
#' @examples
#' 
#' pipeline <- newPipeline(label="example pipeline", cv="cv", nfolds=10, p=0.80)
#' addModule(pipeline, type="M1", label="module 1")
#' addTask(pipeline, "module 1", label="task 1", method=function(x){return(x[1:5,])})

addTask <- function(pipeline,module,label,method,datatype="microarray",parameters=NULL,libraries=NULL,control=NULL){
  
  if(!"Pipeline"%in%class(pipeline)){
    stop("parameter 'pipeline' must be of class 'Pipeline'")
  }
  
  module.names <- names(pipeline$modules)
  
  if(!module%in%module.names){
    stop("parameter 'module' must be one of: '",paste0(module.names,collapse="', '"),"'")
  }
  
  pipeline$modules[[module]]$addTask(label,method,datatype,parameters,libraries,control)
}