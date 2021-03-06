# Function to create the model index for a Pipeline object
# 
# More detailed explaination here
# 
# @param pipeline An object of class \code{Pipeline}
# 
# @return a \code{data.frame} with \code{nrow} equal to the number of unique models and \code{ncol} equal to the number of modules
# 
# @examples
# \dontrun{
# indexModels(pipeline)
# }
# @keywords internal

indexModels <- function(pipeline){
  
  # count the tasks
  task.count <- countTasks(pipeline)
  
  # if any modules have no tasks, return NULL
  if(any(task.count==0)){
    model.index <- NULL
  } else {
    
    # create the parameter list
    params <- lapply(as.numeric(task.count),function(x) 1:x)
    
    # create the index matrix
    model.index <- calculateIndex(params)
    
    # add names to the rows and columns
    colnames(model.index) <- names(task.count)
    row.names(model.index) <- paste0("Model_",1:nrow(model.index))
    
  }
  
  # return model.index
  return(model.index)
  
}