#' Validate that the input method conform to required standards of the Task class
#' 
#' More detailed explaination here
#' 
#' @param method A user-defined \code{function}. This \code{function} must contain input parameters \code{x} (a numerical matrix with \code{n} columns and \code{m} rows), \code{y} (a two-level factor or character vector of length \code{n}), and [optionally] \code{data} (a \code{data.frame} with \code{n} rows, each corresponding to a respective column in \code{x}). The user can provide additional parameters to the \code{function} but must specify default values in the \code{function}'s definition. \code{method} must return a numerical matrix with the same number of columns (\code{ncol}) and column names (\code{colnames}) as \code{x} and [optionally] a rank vector ranking the rows of \code{x} in terms of their importance (see example below).
#' 
#' @return Function will execute silently if no errors are detected in \code{libraries}
#' 
#' @examples
#' set.seed(1234)
#' x <- matrix(rnorm(100),nrow=10)
#' colnames(x) <- paste0("sample_",1:10)
#' row.names(x) <- paste0("gene_",1:10)
#' y <- sample(c("Case","Control"),10,replace=T)
#' 
#' method <- function(x,y){
#'  x[,y=="Case"] <- x[,y=="Case"] + 1
#'  x[,y=="Control"] <- x[,y=="Control"] - 1
#'  return(x)
#' }
#' validateMethodM2(method)
#' 
#' dat <- data.frame(
#'  ID = paste("Sample",1:10,sep="_"),
#'  CovarA = sample(c(0,1),10,replace=T),
#'  CovarB = sample(c(0,1),10,replace=T)
#' )  
#' 
#' method <- function(x,y,dat){
#'  x[,(y=="Case" & dat$CovarA==0)] <- x[,(y=="Case" & dat$CovarA==0)] + 1
#'  x[,(y=="Control" & dat$CovarA==1)] <- x[,(y=="Control" & dat$CovarA==1)] + 1
#'  return(x)
#' }
#' validateMethodM2(method)
#' 
#' method <- function(x,y,dat){
#'  x[,(y=="Case" & dat$CovarA==0)] <- x[,(y=="Case" & dat$CovarA==0)] + 1
#'  x[,(y=="Control" & dat$CovarA==1)] <- x[,(y=="Control" & dat$CovarA==1)] + 1
#'  ranks <- 1:10
#'  return(list(x=x,rank=ranks))
#' }
#' validateMethodM2(method)

validateMethodM2 <- function(method=NULL){
  
  # check that the user defined method
  if(is.null(method)){
    stop("method must be defined")
  }
  
  # check that method is a function
  if(!is.function(method)){
    stop("method must be a function")
  }
  
  # check that method has input parameters x, y, and optionally data
  if(!all(c("x","y")%in%names(formals(method)))){
    stop("method must have input parameters 'x' and 'y'")
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
  
  # check if y has a default value, if so, issue a warning to the user
  if(!is.null(f[["y"]])){
    warning("'y' should not have a default value. The default provided will be disregarded when invoking the method")
  }
  
  # check if data has a default value, if so, issue a warning to the user
  if(!is.null(f[["data"]])){
    warning("'data' should not have a default value. The default provided will be disregarded when invoking the method")
  }
  
  # if there are additional input parameters to method, make sure they have defaults
  # by default, formals() returns parameters without defaults as a blank character string: ""
  # here, I change "" to NULL and check for is.null, however if a user-defined parameter was
  # to actually have a default value of "", this would get overwritten. This is a known limitation.
  
  # check that all parameters other than x, y, and data have defaults, if not, issue error
  cond <- f[-which(names(f)%in%c("x","y","data"))]
  if(length(cond)>0){
    if(any(unlist(lapply(cond,function(x) is.null(x))))){
      stop("All parameters other than 'x' and 'y', and 'data' in 'method' must have default values")
    }    
  }

}









