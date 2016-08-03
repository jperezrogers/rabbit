# Validate that input libraries conform to required standards of the Task class
# 
# More detailed explaination here
# 
# @param libraries A character vector of libraries
# 
# @return Function will execute silently if no errors are detected in \code{libraries}
# 
# @examples
# \dontrun{
# validateLibraries("limma")
# validateDatatype(c("limma","caret"))
# }
# 
# @keywords internal

validateLibraries <- function(libraries=NULL){
  
  # if libraries is not null
  if(!is.null(libraries)){
    
    # check that libraries is a character vector
    if(!is.character(libraries)){
      stop("libraries must be a character vector")
    }
    
    # check that all libraries are installed
    if(!all(libraries%in%installed.packages()[,1])){
      not.libs <- libraries[which(!(libraries%in%installed.packages()[,1]))]
      not.libs.str <- paste(not.libs,collapse=", ")
      stop(paste("The following libraries are not installed. Please install them:",not.libs.str))
    }
  }
}