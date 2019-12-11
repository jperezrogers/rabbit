#' Create a new Module within an existing Pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param type the type of module to add. One of 'M1', 'M2', or 'M3'
#' @param label a character string
#' 
#' @examples
#' \dontrun{
#' pipeline <- newPipeline(label="example pipeline",cv="cv",nfolds=10,p=0.80)
#' addModule(pipeline,type="M1",label="module 1")
#' }
#' 
#' @export

addModule <- function(pipeline,type,label){
  if(!"Pipeline"%in%class(pipeline)){
    stop("parameter 'pipeline' must be of class 'Pipeline'")
  } else {
    pipeline$addModule(type,label)
  }
}