#' Function to count the total number of unique tasks in a Pipeline object
#' 
#' More detailed explaination here
#' 
#' @param pipeline An object of class \code{Pipeline}
#' 
#' @return A named numeric vector
#' 
#' @examples
#' .countTasks(pipeline)

.countTasks = function(pipeline){
  
  # calculate the total number of tasks per module
  task.count <- list()
  module.index <- 0
  for(module in pipeline$modules){
    module.index <- module.index + 1
    ntasks <- 0
    for(task in module$tasks){
      o <- expand.grid(task$control,stringsAsFactors=F)
      if(nrow(o)==0){
        count <- 1
      } else {
        count <- nrow(o)
      }
      ntasks <- ntasks + count
    }
    task.count[module$label] <- ntasks
  }
  
  # return self
  return(unlist(task.count))
  
}