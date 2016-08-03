# Class providing the Task object
# 
# Here is a more detailed sentence
# about the Task class
#
# @docType class
# @keywords biomarker classification
# @return An object of class \code{Task} with members required to execute a discrete function
# @format \code{\link{R6Class}} object
# @import R6
# 
# @field label The name of the task
# @field method A function to perform
# @field parameters Additional input parameters used in \code{method}
# @field libraries A character vector of libraries required to run the \code{method}
# @field control A named list with values of \code{parameters} to pass to \code{method}
# @field datatype A character vector of valid datatypes on which \code{method} can be used
# 
# 
# @keywords internal

# define the Task class
Task <- R6::R6Class("Task",
  
  public = list(
    
    # public members
    label = NA,
    method = NA,
    datatype = NA,
    parameters = NA,
    libraries = NA,
    control = NA,
    
    # public methods
    initialize = function(label,method,datatype,parameters=NULL,libraries=NULL,control=NULL){
      self$label = label
      self$method = method
      self$datatype = datatype
      self$parameters = parameters
      self$libraries = libraries
      self$control = control
    }
    
  ),
  
  lock_class = TRUE
  
)

