#' Create a new Task within a Module in an existing Pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param module the label of the module that will contain the task
#' @param label a character string
#' @param method a user-defined function. The input/output requirements for \code{method} are dependent upon the class of \code{module}. See details below for more information. 
#' @param parameters Additional input parameters used in \code{method}
#' @param libraries A character vector of libraries required in \code{method}
#' @param control A named list with values of \code{parameters} to pass to \code{method}
#' @param datatype A character vector of valid datatypes on which \code{method} can be used
#' 
#' @details
#' Specific input parameters and outputs are required for user-defined methods depending on the class of \code{module}. Tasks added to \code{M1} modules are required to have input parameter \code{x}. Tasks added to \code{M2} modules are required to have input parameters \code{x}, \code{y}, and optionally \code{data}. Tasks added to \code{M3} modules are required to have input parameters \code{x} and \code{rank}. Tasks added to \code{M4} modules are required to have input parameters \code{x}, \code{y}, and \code{testdata}.
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