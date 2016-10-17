#' Get the function call used to add a task to a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param module the label of a module within \code{pipeline}
#' @param task a character string or vector. The label(s) of a task or tasks in \code{module} to delete. 
#' 
#' @examples
#' \dontrun{
#' getCall(pipeline,module,task)
#' }
#' @export

getCall <- function(pipeline, module, task){
  
  if(!"Pipeline"%in%class(pipeline)){
    stop("parameter 'pipeline' must be of class 'Pipeline'")
  }
  
  module.names <- names(pipeline$modules)
  
  if(!module%in%module.names){
    stop("parameter 'module' must be one of: '",paste0(module.names,collapse="', '"),"'")
  }
  
  task.names <- names(pipeline$modules[[module]]$tasks)
  
  if(!task%in%task.names){
    stop("parameter 'task' must be one of: '",paste0(task.names,collapse="', '"),"'")
  }

  task.call <- list()
  task.obj <- pipeline$modules[[module]]$tasks[[task]]
  task.call$pipeline <- pipeline
  task.call$module <- module
  task.call$label <- task.obj$label
  task.call$method <- task.obj$method
  task.call$parameters <- task.obj$parameters
  task.call$control <- task.obj$control
  task.call$libraries <- task.obj$libraries
  # task.call$datatype <- task.obj$datatype
  
  return(task.call)
}