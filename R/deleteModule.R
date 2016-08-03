#' Delete a module from a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param module the label of a module within \code{pipeline}
#' 
#' @examples
#' \dontrun{
#' deleteModule(pipeline,module)
#' }
#' @export

deleteModule <- function(pipeline,module){
  pipeline$deleteModule(label=module)
}