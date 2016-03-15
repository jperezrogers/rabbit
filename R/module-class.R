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
#' @field class The class of the Module (one of \code{M1}, \code{M2}, \code{M3}, \code{M4})
#' @field tasks A list of Task objects
#' @field active A Boolean vector of length \code{length(tasks)}
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{addTask(label,method,parameters=NA,libraries=NA,control=NA,datatype)}}{Add a Task to the Module}
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
    
    # public members
    label = NA, # the name of the Module
    
    # public methods
    addTask = function(){ # a function to create a new Task and add it to the Module (add or addTask?)
      invisible(self)
    },
    deleteTask = function(){ # a funtion to delete a Task from a Module (delete or deleteTask?)
      invisible(self)
    },
    run = function(){ # a function to execute a Task
      invisible(self)
    },
    activate = function(){ # a function to turn on a task or tasks
      invisible(self)
    },
    deactivate = function(){ # a function to turn off a task or tasks
      invisible(self)
    },
    summary = function(){ # a function that will print out a summary of the Tasks in the Module
      invisible(self)
    }

  ),
  
  private = list(
    
    # private members
    class = NA, # the child class of the Module
    tasks = NA, # a list of Task objects
    active = NA, # a Boolean vector of length = number of Tasks
    
    # private methods
    addTaskMember = function(){ # a function to add a Task as a member of the Module object
      invisible(self)
    }
    
  ),
  
  lock_class = TRUE
  
)

