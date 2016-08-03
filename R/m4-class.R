# define the M1 class
M4 <- R6::R6Class("M4",
  
  inherit = Module,
  
  public = list(
    
    #================#
    # public members #
    #================#
    
    #================#
    # public methods #
    #================#
    
    # initialization function for all submodules
    initialize = function(label=NULL){
      
      # check that label is provided and in the right format, if so, set self$label
      if(is.null(label)){
        stop("'label' must be provided")
      } else if(!is.character(label)){
        stop("'label' must be of class character")
      } else {
        self$label <- label
      }
      
      # set the task list to be empty
      self$tasks <- list()
      
      # assign the class of the object to be M1 (might not need this)
      private$class <- "M4"
      
    }
    
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
      validateLabel(label=task$label)
      validateMethodM4(method=task$method)
      validateParameters(parameters=task$parameters)
      validateLibraries(libraries=task$libraries)
      validateControl(parameters=task$parameters,control=task$control)
      validateDatatype(datatype=task$datatype)
    }
    
  ),
  
  lock_class = FALSE
  
)

