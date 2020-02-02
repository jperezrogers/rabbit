#' Update the configuration parameters of a Pipeline object
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param what what to update? One of \code{label}, \code{cv}, \code{nfolds}, or \code{p}.
#' @param value value to update the variable named by \code{what}
#' 
#' @examples
#' \dontrun{
#' update(pipeline=pipeline, what="cv", value="lgocv")
#' }
#' 
#' @export

update <- function(pipeline, what, value){
  
  # check that pipeline of class Pipeline
  if(!"Pipeline"%in%class(pipeline)){
    stop("parameter 'pipeline' must be an object of class 'Pipeline'")
  }
  
  # check that what is a valid pipeline parameter
  if(!what%in%c("label","cv","nfolds","p")){
    stop("parameter 'what' must be one of 'label', 'cv', 'nfolds', or 'p'")
  }
  
  # if what = label
  if(what=="label"){
    if(is.null(value)){
      stop("parameter 'label' must not be NULL")
    } else if(!is.character(value)){
      stop("parameter 'label' must be of class character")
    } else {
      pipeline$.setPrivate("label",value)
    }
  }
  
  # if what = cv
  if(what=="cv"){
    msg <- "parameter 'cv' must be one of 'cv', 'loocv', 'lgocv', or 'boot'"
    if(!is.character(value)){
      stop(msg)
    } else if(length(value)!=1){
      stop(msg)
    } else if(!value%in%c("cv","loocv","lgocv","boot")){
      stop(msg)
    } else {
      pipeline$.setPrivate("cv",value)
    }
  }
    
  # if what = nfolds
  if(what=="nfolds"){
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if(!is.numeric(value)){
      stop("parameter 'nfolds' must be of class numeric")
    } else if(!is.wholenumber(value)){
      stop("parameter 'nfolds' must be an integer >=1")
    } else {
      pipeline$.setPrivate("nfolds",value)
    }
  }
  
  # if what = p
  if(what=="p"){
    if(pipeline$.getPrivate("cv")%in%c("lgocv","boot")){
      if(!is.numeric(value)){
        stop("parameter 'p' must be a numeric value between 0 and 1")
      } else if(value>=1|value<=0){
        stop("parameter 'p' must be a numeric value between 0 and 1")
      }
    }
    pipeline$.setPrivate("p",value)
  }
}






