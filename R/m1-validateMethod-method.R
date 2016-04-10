#' Validate that the input method conform to required standards of the Task class
#' 
#' More detailed explaination here
#' 
#' @param method A user-defined \code{function}. This \code{function} must contain input parameter \code{x} (a numerical matrix). The user can provide additional parameters to the \code{function} but must specify default values in the \code{function}'s definition. \code{method} must return a numerical matrix with the same number of columns (\code{ncol}) and column names (\code{colnames}) as \code{x} (see example below).
#' 
#' @return Function will execute silently if no errors are detected in \code{libraries}
#' 
#' @examples
#' set.seed(1234)
#' x <- matrix(rnorm(100),nrow=10)
#' colnames(x) <- paste0("sample_",1:10)
#' row.names(x) <- paste0("gene_",1:10)
#' 
#' method <- function(x){
#'  x <- x**2
#'  x <- x[1:5,]
#'  return(x)
#' }
#' validateMethodM1(method)
#' 
#' method <- function(x,pwr=3){
#'  x <- x**pwr
#'  x <- x[1:5,]
#'  return(x)
#' }
#' validateMethodM1(method,pwr=3)

validateMethodM1 <- function(method=NULL){
  
  # check that the user defined method
  if(is.null(method)){
    stop("method must be defined")
  }
  
  # check that method is a function
  if(!is.function(method)){
    stop("method must be a function")
  }
  
  # check that method has input parameter x
  if(!("x"%in%names(formals(method)))){
    stop("method must have input parameter 'x'")
  }

  # extract parameters and defaults from method, setting params with no default to NULL
  f <- lapply(formals(method), function(x){
    if(!is.null(x)){
      if(is.name(x) & !nzchar(x)) NULL else x
    }
  })
  
  # check if x has a default value, if so, issue a warning to the user
  if(!is.null(f[["x"]])){
    warning("'x' should not have a default value. The default provided will be disregarded when invoking the method")
  }
  
  # if there are additional input parameters to method, make sure they have defaults
  # by default, formals() returns parameters without defaults as a blank character string: ""
  # here, I change "" to NULL and check for is.null, however if a user-defined parameter was
  # to actually have a default value of "", this would get overwritten. This is a known limitation.
  
  # check that all parameters otehr than x have defaults, if not, issue error
  if(length(f)>1){
    if(any(unlist(lapply(f[-which(names(f)=="x")],function(x) is.null(x))))){
      stop("All parameters other than 'x' in 'method' must have default values")
    }
  }
}












