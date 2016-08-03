# Validate that input label conforms to required standards of the Task class
# 
# More detailed explaination here
# 
# @param label A character string
# 
# @return Function will execute silently if no errors are detected in \code{label}
# 
# @examples
# \dontrun{
# validateLabel("label")
# }
# 
# @keywords internal

validateLabel <- function(label=NULL){
  
  # check if label is NULL
  if(is.null(label)){
    stop("The label of a Task cannot be NULL")
  }
  
  # check that label is of length 1
  if(length(label)>1){
    stop("The label of a Task must be a character vector of length 1")
  }
  
  # check that the label is of class character
  if(!is.character(label)){
    stop("The label of a Task must be of class character")
  }
  
}