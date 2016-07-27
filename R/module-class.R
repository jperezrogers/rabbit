#' Class providing the Module object
#' 
#' Here is a more detailed sentence
#' about the Module class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords biomarker classification
#' @return An object of class \code{Module}
#' @format \code{\link{R6Class}} object
#' 
#' @field label The name of the Module
#' @field tasks A list of Task objects
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{addTask(label,method,datatype,parameters,control,libraries)}}{Add a Task to the Module}
#'    \item{\code{deleteTask(label)}}{Remove a Task from the Module}
#'    \item{\code{run()}}{Execute a Task}
#'    \item{\code{activate(label)}}{Activate a Task}
#'    \item{\code{deactivate(label)}}{Deactivate a Task}
#'    \item{\code{summary()}}{Print out a summary of the Tasks in the Module}
#' }
#' 
#' @examples
#' 
#' 

# define the Module class
Module <- R6Class("Module",
  
  public = list(
    
    #================#
    # public members #
    #================#
    
    label = NA, # the name of the Module
    tasks = list(), # a list of Task objects
    
    #================#
    # public methods #
    #================#
    
    # global function to add a task
    addTask = function(label=NULL,method=NULL,datatype=NULL,parameters=NULL,libraries=NULL,control=NULL){
      
      # create a new Task object
      task <- Task$new(label,method,datatype,parameters,libraries,control)
      
      # validate the object's parameters
      private$validate(task)
      
      # if there are no errors, the above function will execute silently
      self$tasks[[label]] <- task
      
      # update the active task list
      private$active[[label]] <- TRUE
      
      # return self
      invisible(self)
    },
    
    # global function to delete a task
    deleteTask = function(label=NULL){
      
      # check that label is not null
      if(is.null(label)){
        stop("argument 'label' cannot be NULL")
      }
      
      # check that label is a character string
      if(!is.character(label)){
        stop("argument 'label' must be of class character")
      }
      
      # check that all values provided to label are true labels of tasks
      if(!all(label%in%names(self$tasks))){
        not.labels <- label[which(!(label%in%names(self$tasks)))]
        if(length(not.labels)==1){
          msg <- paste0(not.labels," is not a valid task label")
        } else {
          msg <- paste0(paste(not.labels,collapse=", ")," are not valid task labels")
        }
        stop(msg)
      }
      
      # remove the task from the module
      self$tasks[label] <- NULL
      
      # remove the task from the active list
      private$active[label] <- NULL
      
      # if there are no more tasks in the module, restore to default
      if(length(self$tasks)==0){
        self$tasks <- list()
      }
      
      # return self
      invisible(self)
    },
    
    # global function to activate a task
    activateTask = function(label){
      
      # check that label is not null
      if(is.null(label)){
        stop("argument 'label' cannot be NULL")
      }
      
      # check that label is a character string
      if(!is.character(label)){
        stop("argument 'label' must be of class character")
      }
      
      # check that all values provided to label are true labels of tasks
      if(!all(label%in%names(self$tasks))){
        not.labels <- label[which(!(label%in%names(self$tasks)))]
        if(length(not.labels)==1){
          msg <- paste0(not.labels," is not a valid task label")
        } else {
          msg <- paste0(paste(not.labels,collapse=", ")," are not valid task labels")
        }
        stop(msg)
      }
      
      # activate the task(s)
      private$active[label] <- TRUE
      
      # return self
      invisible(self)
      
    },
    
    # global function to deactivate a task
    deactivateTask = function(label){
      
      # check that label is not null
      if(is.null(label)){
        stop("argument 'label' cannot be NULL")
      }
      
      # check that label is a character string
      if(!is.character(label)){
        stop("argument 'label' must be of class character")
      }
      
      # check that all values provided to label are true labels of tasks
      if(!all(label%in%names(self$tasks))){
        not.labels <- label[which(!(label%in%names(self$tasks)))]
        if(length(not.labels)==1){
          msg <- paste0(not.labels," is not a valid task label")
        } else {
          msg <- paste0(paste(not.labels,collapse=", ")," are not valid task labels")
        }
        stop(msg)
      }
      
      # deactivate the task(s)
      private$active[label] <- FALSE
      
      # return self
      invisible(self)
    },
    
    getActive = function(){
      return(private$active)
    }

  ),
  
  private = list(
    
    #=================#
    # private members #
    #=================#
    
    class = NA, # the child class of the Module
    active = list(), # a Boolean vector of length = number of Tasks
    
    #=================#
    # private methods #
    #=================#
    
    # global placeholder for validate function. This method is reestablished in submodules
    validate = function(task){
      invisible(self)
    }

  ),
  
  active = list(
    
    summary = function(){ # a function that will print out a summary of the Tasks in the Module
      invisible(self)
    },
    
    getClass = function(){
      return(private$class)
    }
    
  ),
  
  lock_class = FALSE
  
)

