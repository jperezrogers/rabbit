# load the required libraries
library(R6)

# define the M2 class
M2 <- R6Class("M2",
  
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

