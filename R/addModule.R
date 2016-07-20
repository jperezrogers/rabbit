#' Create a new Module within an existing Pipeline
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
#' pipeline <- newPipeline(label="example pipeline",cv="cv",nfolds=10,p=0.80)
#' addModule(pipeline,type="M1",label="module 1")

addModule <- function(pipeline,type,label){
  if(!"Pipeline"%in%class(pipeline)){
    stop("parameter 'pipeline' must be of class 'Pipeline'")
  } else {
    pipeline$addModule(type,label)
  }
}