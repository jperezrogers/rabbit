# load the required libraries
library(R6)

# define the M1 class
M1 <- R6Class("M1",
  
  inherit = Module,
  
  public = list(
    
    # public members
    
    # public methods
    initialize = function(label,class="M1"){
      self$label <- label
      private$class <- class
    },
    # a function to create a new Task and add it to the Module (add or addTask?)
    addTask = function(label,method,datatype,parameters=NULL,libraries=NULL,control=NULL){ 
      task <- Task$new(label,method,datatype,parameters,libraries,control)
      if(validate(task)){
        private$tasks[[label]] <- task
      }
      invisible(self)
    }
    
  ),
  
  private = list(
    
    # private members
    
    # private methods
    validate = function(){
      invisible(self) 
    }
    
  ),
  
  lock_class = FALSE
                  
)

