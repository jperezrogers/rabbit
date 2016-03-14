#' The definition of the Task class
#' 
#' Here is a more detailed sentence
#' about the Task class
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

# define the Task class
Task <- R6Class("Task",
  
  public = list(
    
    # public members
    label = NA,
    method = NA,
    parameters = NA,
    libraries = NA,
    control = NA,
    datatype = NA,
    
    # public methods
    initialize = function(label,method,parameters=NULL,libraries=NULL,control=NULL,datatype){
      self$label = label
      self$method = method
      self$parameters = parameters
      self$libraries = libraries
      self$control = control
      self$datatype = datatype
      self$testdata = testdata
      self$moduletype = moduletype
      private$validateParameters(self$parameters)
    }
    
  )
)

