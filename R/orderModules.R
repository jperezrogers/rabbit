#' Order the modules within a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param order a character vector of module labels
#' 
#' @examples
#' \dontrun{
#' orderModules(pipeline,order)
#' }
#' @export

orderModules <- function(pipeline,order){
  pipeline$orderModules(x=order)
}