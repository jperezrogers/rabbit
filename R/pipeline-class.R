#' Class providing the Pipeline object
#' 
#' Here is a more detailed sentence
#' about the Pipeline class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords biomarker classification
#' @return An object of class \code{Pipeline}
#' @format \code{\link{R6Class}} object
#' 
#' @usage Pipeline$new(label, order = NULL, cv = "cv", nfolds = 10, p = NULL)
#' 
#' @field label The name of the Module
#' @field order An ordered character vector of \code{Module} names. \code{order} determined the order in which \code{Modules} will be run in the pipeline
#' @field cv The resampling method to be used for cross-validation. One of \code{"cv"}, \code{"loocv"}, \code{"lgocv"}, \code{"boot"}, or \code{"boot632"}
#' @field nfolds The number of folds for k-fold cross-validation or the number of resamplings for \code{"lgocv"}, \code{"boot"}, or \code{"boot632"}
#' @field p The percentage of samples in the internal training set for \code{"lgocv"}
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{addModule(type)}}{Add a Module to the Pipeline}
#'    \item{\code{orderModules()}}{Change the order in which Modules are executed}
#' }
#' 
#' @examples
#' 

Pipeline <- R6Class("Pipeline",
  
  public = list(
  
    #================#
    # public members #
    #================#
    
    label = NA, # the name of the pipeline,
    modules = list(), # a list of modules in the pipeline
    cv = NA, # resampling method
    nfolds = NA, # number of folds or resampling iterations
    p = NA, # percentage of samples in the training set
    
    #================#
    # public methods #
    #================#
    
    initialize = function(label=NULL,cv="cv",nfolds=10,p=0.80){
      # maybe pipelines should come stock with an M4 module that cannot be moved
      # check that label is provided and in the right format, if so, set 
      # self$label
      if(is.null(label)){
        stop("parameter 'label' must not be NULL")
      } else if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      } else {
        self$label <- label
      }
      
      # check that cv is valid
      if(!cv%in%c("cv","loocv","lgocv","boot","boot632")){
        stop("parameter 'cv' must be one of 'cv', 'loocv', 'lgocv', 'boot', or 'boot632'")
      }
      
      # check that nfolds is an integer
      is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
      if(!is.wholenumber(nfolds)){
        stop("parameter 'nfolds' must be an integer >1")
      } else {
        self$nfolds <- nfolds
      }
      
      # check that p is a numeric between 0 and 1
      if(cv%in%c("lgocv","boot","boot632")&!is.numeric(p)){
        stop("parameter 'p' must be a numeric value in the range [0,1]")
      } else if(p>=1|p<=0){
        stop("parameter 'p' must be a value in the range [0,1]")
      } else {
        self$p <- p
      }
      
      # set the task list to be empty
      self$modules <- list()
      
      # add an M4 module to the pipeline
      self$modules[["classification"]] <- M4$new(label="classification")
      
    },
    
    addModule = function(type=NULL,label=NULL){
      
      # check that type is not NULL
      if(is.null(type)){
        stop("parameter 'type' cannot be NULL")
      }
      
      # check that type is one of the available module types
      # I originally had M4 in here but there should only ever be one M4 module 
      # and it comes last so the user shouldnt be able to add it / delete it / 
      # move it
      if(!type%in%c("M1","M2","M3")){
        stop("parameter 'type' must be one of 'M1', 'M2', or 'M3'")
      }
      
      # check that label is not NULL
      if(is.null(label)){
        stop("parameter 'label' cannot be NULL")
      } 
      
      # check that label is a character
      if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      }
      
      # check that label is of length 1
      if(!is.character(label)){
        stop("parameter 'label' must be of length 1")
      }
      
      # if all of the above pass, add a new module to the pipeline 
      if(type=="M1"){
        self$modules[[label]] <- M1$new(label=label)
      } else if(type=="M2"){
        self$modules[[label]] <- M2$new(label=label)
      } else if(type=="M3"){
        self$modules[[label]] <- M3$new(label=label)
      }
      
    },
    
    orderModules = function(order){
      invisible(self)
    }
    
    
  ),
  
  private = list(
    
    #=================#
    # private members #
    #=================#
    
    order = NA,
    active = list()
    
    #=================#
    # private methods #
    #=================#
    
    
  ),
  
  active = list(
    
  ),
  
  lock_class=FALSE
  
)
























