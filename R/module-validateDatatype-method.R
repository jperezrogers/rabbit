# Validate that input datatype conforms to required standards of the Task class
# 
# More detailed explaination here
# 
# @param datatype One or more of \code{microarray}, \code{count}, \code{rpkm}
# 
# @return Function will execute silently if no errors are detected in \code{datatype}
# 
# @examples
# \dontrun{
# validateDatatype("microarray")
# validateDatatype(c("microarray","count"))
# validateDatatype(c("microarray","count","rpkm"))
# }
# 
# @keywords internal

validateDatatype <- function(datatype=NULL){
  
  # check if datatype is NULL
  if(is.null(datatype)){
    stop("The datatype of a Task cannot be NULL")
  }
  
  # check that datatype is a character
  if(!all(sapply(datatype,is.character))){
    stop("The datatype of a Task must be a character vector")
  }
  
  # check that datatype is in c("microarray","count","rpkm")
  if(!all(datatype%in%c("microarray","count","rpkm"))){
    stop("The datatype of a Task must be one or more of 'microarray', 'count', 'rpkm'")
  }
  
}