# load the required libraries
library(R6)

# define the M1 class
M2 <- R6Class("M2",
  
  inherit = Module,
  
  public = list(
  
    #================#
    # public members #
    #================#
    
    #================#
    # public methods #
    #================#
    
  ),
  
  private = list(
    
    #=================#
    # private members #
    #=================#
    
    #=================#
    # private methods #
    #=================#
    
    # validate all of the components of the Task
    validate = function(task){
      validateLabel(task$label)
      validateMethodM2(task$method)
      validateParameters(task$parameters)
      validateLibraries(task$libraries)
      validateControl(task$control)
      validateDatatype(task$datatype)
    }
    
  ),
  
  lock_class = FALSE
              
)

