#' Deactivate a task within a module in a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param module the label of a module within \code{pipeline}
#' @param task a character string or vector. The label(s) of a task or tasks in \code{module} to deactivate. 
#' 
#' @examples
#' \dontrun{
#' deactivateTask(pipeline,module,task)
#' }
#' @export

deactivateTask <- function(pipeline,module,task){
  
  module.names <- names(pipeline$modules)
  
  if(!module%in%module.names){
    stop("parameter 'module' must be one of: '",paste0(module.names,collapse="', '"),"'")
  }
  
  pipeline$modules[[module]]$deactivateTask(task)
}