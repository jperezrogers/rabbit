#' Create an object of class Pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param label a character string
#' @param cv the cross-validation methodology to employ. One of 'cv', 'loocv', 'lgocv', or 'boot'
#' @param nfolds the number of folds to use in k-fold, leave-group out, or bootstrapped cross-validation
#' @param p the proportion of samples included in the training set in each fold
#' 
#' @return an object of class \code{Pipeline}
#' 
#' @examples
#' \dontrun{
#' newPipeline(label="example pipeline",cv="lgocv",nfolds=10,p=0.80)
#' }
#' @export

newPipeline <- function(label,cv="lgocv",nfolds=10,p=0.80){
  x <- Pipeline$new(label,cv,nfolds,p)
  return(x)
}
