# Validate that input control conform to required standards of the Task class
# 
# More detailed explaination here
# 
# @param parameters A \code{data.frame} with columns \code{parameter}, \code{class}, and \code{label}
# @param control A \code{list} with one or more named elements. Each element name must be equal to one of the values of \code{parameters$parameter}. The values included in \code{control} should be distinct instances of the parameter to be passed to the \code{method} of an object of class \code{Task}.
# 
# @return Function will execute silently if no errors are detected in \code{libraries}
# 
# @examples
# \dontrun{
# parameters <- data.frame(
#  parameter = c("paramA","paramB"),
#  class = c("character","character"),
#  label = c("myParamA","myParamB"),
#  stringsAsFactors=F
# )
# control <- list(
#  paramA = c("str1","str2","str3")
# )
# validateControl(parameters,control)
# }
# 
# @keywords internal


validateControl <- function(control=NULL,parameters=NULL){
  
  # check if control is null, if so, exit, if not, proceed
  if(!(is.null(control))){
    
    # check that parameters is defined and if it is, validate it
    if(is.null(parameters)){
      stop("parameters cannot be NULL if control is defined")
    } else {
      validateParameters(parameters)
    }
    
    # now that parameters are valid and control is defined, check that control is a list
    # if("list"%in%is(control)&!"data.frame"%in%is(control)){
    if(class(control)!="list"){
      stop("parameter 'control' must be of class 'list'")
    }
    
    # check that control has length greater than zero
    if(length(control)==0){
      stop("control must be a list of length > 0")
    }
    
    # check that control is a named list
    if(is.null(names(control))){
      stop("control must be a named list")
    }
    
    # check that the names in control are all unique
    if(length(unique(names(control)))!=length(control)){
      stop("All named elements in control must be unique")
    }
    
    # all named elements in control must be equal to something in parameters$parameter
    if(!all(names(control)%in%parameters$parameter)){
      stop("Not all named elements in control are in the parameter slot of parameters")
    }
    
    # check that each named element in control is not null
    if(any(unlist(lapply(control,is.null)))){
      stop("Named elements in control cannot be NULL")
    }
    
    # all values in each list item in control must be of the class provided in parameters$class
    for(i in 1:length(control)){
      param <- names(control)[i]
      param.class <- parameters$class[which(parameters$parameter==param)]
      if(!all(sapply(control[[i]],function(x) is(x,param.class)))){
        stop(paste0("Not all of the values provided for parameter '",param,"' in control are of class '",param.class,"'"))
      }
    }  
  }
}