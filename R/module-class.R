#' The definition of the Module class
#' 
#' Here is a more detailed sentence
#' about the Module class
#'
#' @param name description
#' @param name description
#' 
#' @return
#'
#' @keywords
#'
#' @export
#' 
#' @examples
#'

# load the required libraries
library(R6)

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

