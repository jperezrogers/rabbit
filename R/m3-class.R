# load the required libraries
library(R6)

# define the M3 class
M3 <- R6Class("M3",
  
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

