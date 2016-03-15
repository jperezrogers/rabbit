# load the required libraries
library(R6)

# define the M4 class
M4 <- R6Class("M4",
  
  inherit = Module,
  
  public = list(
    
    # public members
    
    # public methods
    
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

