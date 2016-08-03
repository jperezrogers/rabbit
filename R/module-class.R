# Class providing the Module object
# 
# Here is a more detailed sentence
# about the Module class
#
# @docType class
# @keywords biomarker classification
# @return An object of class \code{Module}
# @format \code{\link{R6Class}} object
# @import R6
# 
# @field label the name of the Module
# @field tasks a list of active Task objects
# @field inactive a list of inactive Task objects
# 
# @section Methods:
# \describe{
#    \item{\code{addTask(label,method,datatype,parameters,control,libraries)}}{Add a Task to the Module}
#    \item{\code{deleteTask(label)}}{Remove a Task from the Module}
#    \item{\code{run()}}{Execute a Task}
#    \item{\code{activate(label)}}{Activate a Task}
#    \item{\code{deactivate(label)}}{Deactivate a Task}
#    \item{\code{summary()}}{Print out a summary of the Tasks in the Module}
# }
#
# @keywords internal

# define the Module class
Module <- R6::R6Class("Module",
  
  public = list(
    
    #================#
    # public members #
    #================#
    
    label = NA, # the name of the Module
    tasks = list(), # a list of Task objects
    inactive =list(), # a list of inactive Task objects
    
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
      if(!all(label%in%c(names(self$tasks),names(self$inactive)))){
        not.labels <- label[which(!(label%in%c(names(self$tasks),names(self$inactive))))]
        if(length(not.labels)==1){
          msg <- paste0(not.labels," is not a valid task label")
        } else {
          msg <- paste0(paste(not.labels,collapse=", ")," are not valid task labels")
        }
        stop(msg)
      }
      
      # remove the task from the module
      self$tasks[label] <- NULL
      self$inactive[label] <- NULL
      
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
      if(!all(label%in%c(names(self$tasks),names(self$inactive)))){
        not.labels <- label[which(!(label%in%c(names(self$tasks),names(self$inactive))))]
        if(length(not.labels)==1){
          msg <- paste0(not.labels," is not a valid task label")
        } else {
          msg <- paste0(paste(not.labels,collapse=", ")," are not valid task labels")
        }
        stop(msg)
      }
      
      # check if any of the tasks are already inactive, if so, issue warning
      if(any(label%in%names(self$tasks))){
        idx <- which(label%in%names(self$tasks))
        already.active <- label[idx]
        warning(paste0("the following tasks are already active: ",paste0("'",already.active,"'",collapse=", ")))
        label <- label[-idx]
      }
      
      if(length(label)>0){
        
        # activate the task(s) 
        private$active[label] <- TRUE
        
        # move the task from the active task member to the inactive task member
        for(i in 1:length(label)){
          self$tasks[[label[i]]] <- self$inactive[[label[i]]]
          self$inactive[[label[i]]] <- NULL
        }
        
      }
      
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
      if(!all(label%in%c(names(self$tasks),names(self$inactive)))){
        not.labels <- label[which(!(label%in%names(self$tasks)))]
        if(length(not.labels)==1){
          msg <- paste0("'",not.labels,"' is not a valid task label")
        } else {
          msg <- paste0(paste("'",not.labels,collapse="', "),"' are not valid task labels")
        }
        stop(msg)
      }
      
      # check if any of the tasks are already inactive, if so, issue warning
      if(any(label%in%names(self$inactive))){
        idx <- which(label%in%names(self$inactive))
        already.inactive <- label[idx]
        warning(paste0("the following tasks are already inactive: ",paste0("'",already.inactive,"'",collapse=", ")))
        label <- label[-idx]
      }
      
      
      if(length(label)>0){
        
        # deactivate the task(s) 
        private$active[label] <- FALSE  
        
        # move the task from the active task member to the inactive task member
        for(i in 1:length(label)){
          self$inactive[[label[i]]] <- self$tasks[[label[i]]]
          self$tasks[[label[i]]] <- NULL
        }
        
      }

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

