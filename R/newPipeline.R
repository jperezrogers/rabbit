#' Create a new instance of a Pipeline
#' 
#' Author: Joe Perez-Rogers
#' 
#' @param label a character string
#' @param cv the cross-validation methodology to employ. One of 'cv', 'loocv', 'lgocv', or 'boot'
#' @param nfolds the number of folds to use in k-fold, leave-group out, or bootstrapped cross-validation
#' @param p the proportion of samples included in the training set in each fold
#' 
#' @return an object of class \code{Pipeline}
#' 
#' @examples
#' 
#' newPipeline(label="example pipeline",cv="cv",nfolds=10,p=0.80)

newPipeline <- function(label=NULL,cv="cv",nfolds=10,p=0.80){
  x <- Pipeline$new(label,cv,nfolds,p)
  return(x)
}
