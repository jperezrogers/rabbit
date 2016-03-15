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
    }
    
  ),
  
  private = list(
    
    # private members
    
    # private methods
    validate = function(){
      invisible(self) 
    }
    
  ),
  
  lock_class = TRUE
                  
)

