#' Delete a task from a module within a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param module the label of a module within \code{pipeline}
#' @param task a character string or vector. The label(s) of a task or tasks in \code{module} to delete. 
#' 
#' @examples
#' 
#' need examples

deleteTask <- function(pipeline,module,task){
  pipeline$modules[[module]]$deleteTask(task)
}